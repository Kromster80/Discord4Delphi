unit Unit_Discord;
interface
uses
  SysUtils, Windows,
  Unit_DiscordTypes;

type
  TDiscord4Delphi = class
  private const
    DISCORD_GAME_SDK_DLL_NAME = 'discord_game_sdk 3.2.1.dll';
  private
    fOnLog: TProc<string>;

    fLibHandle: NativeUInt;
    fDLLDiscordCreate: TDLLDiscordCreate;

    fActive: Boolean;
    fApplicationId: Int64;
    fDiscordCore: PDiscordCore;
    fDiscordActivityManager: PDiscordActivityManager;

    procedure DoLog(const aText: string);
    procedure LoadDLL(const aDLLPath: string);
    procedure InitDiscord;
    procedure RunCallbacks;
  public
    constructor Create(aApplicationId: Int64; aOnLog: TProc<string>);
    destructor Destroy; override;

    procedure ActivityChange(const aDetail, aState: string; aActivityStart, aActivityEnd: TDateTime);
    procedure ActivityClear;

    procedure Update;
  end;


implementation
uses
  DateUtils;

var
  fDiscord: TDiscord4Delphi;


procedure OnRelationshipsRefresh(aEvent_data: Pointer); stdcall;
begin
  fDiscord.DoLog('OnRelationshipsRefresh');
end;


procedure OnUserUpdated(aEvent_data: Pointer); stdcall;
begin
  fDiscord.DoLog('OnUserUpdated');
end;


procedure OnLogHook(aHook_data: Pointer; aLevel: TDiscordLogLevel; aMessage: PUTF8Char); stdcall;
begin
  fDiscord.DoLog('OnLogHook - ' + aMessage);
end;


procedure OnActivityCallback(aCallback_data: Pointer; aResult: TDiscordResult); stdcall;
begin
  fDiscord.DoLog(Format('OnActivityCallback - %s (%d)', [DiscordResultString[aResult], Ord(aResult)]));
end;


{ TDiscord4Delphi }
constructor TDiscord4Delphi.Create(aApplicationId: Int64; aOnLog: TProc<string>);
begin
  inherited Create;

  fDiscord := Self;

  fApplicationId := aApplicationId;
  fOnLog := aOnLog;

  // Load DLL dynamically, so we could move it into the utility folder
  try
    LoadDLL(DISCORD_GAME_SDK_DLL_NAME);

    InitDiscord;
  except
    on E: Exception do
    begin
      DoLog(Format('Exception: %s', [E.Message]));
      fActive := False;
    end;
  end;
end;


destructor TDiscord4Delphi.Destroy;
begin
  fActive := False;

  inherited;
end;


procedure TDiscord4Delphi.DoLog(const aText: string);
begin
  if Assigned(fOnLog) then
    fOnLog(aText);
end;


procedure TDiscord4Delphi.LoadDLL(const aDLLPath: string);
var
  err: Cardinal;
begin
  if not FileExists(aDLLPath) then
    raise Exception.Create(Format('Error - %s not found', [aDLLPath]));

  DoLog(Format('Loading "%s"', [aDLLPath]));

  // Load without displaying any popup error messages
  fLibHandle := SafeLoadLibrary(aDLLPath, $FFFF);
  if fLibHandle = 0 then
    raise Exception.Create(Format('DLL was NOT loaded - %d', [GetLastError]));

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('Error in the DLL loading - %d', [err]));

  fDLLDiscordCreate := GetProcAddress(fLibHandle, 'DiscordCreate');

  if not Assigned(fDLLDiscordCreate) then
    raise Exception.Create('Could not get process address in the DLL');

  DoLog(Format('Linked DiscordCreate at "$%.8x"', [PCardinal(Addr(fDLLDiscordCreate))^]));
end;


procedure TDiscord4Delphi.RunCallbacks;
var
  res: TDiscordResult;
begin
  if not fActive then Exit;

  res := fDiscordCore.run_callbacks(fDiscordCore);

  if res <> DiscordResult_Ok then
    DoLog(Format('fDiscordCore.run_callbacks - %s (%d)', [DiscordResultString[res], Ord(res)]));
end;


procedure TDiscord4Delphi.Update;
begin
  RunCallbacks;
end;


procedure TDiscord4Delphi.InitDiscord;
var
  users_events: TDiscordUserEvents;
  activities_events: TDiscordActivityEvents;
  relationships_events: TDiscordRelationshipEvents;
  params: TDiscordCreateParams;
  res: TDiscordResult;
  err: Cardinal;
begin
{
    struct Application app;
    memset(&app, 0, sizeof(app));

    struct IDiscordUserEvents users_events;
    memset(&users_events, 0, sizeof(users_events));
    users_events.on_current_user_update = OnUserUpdated;

    struct IDiscordActivityEvents activities_events;
    memset(&activities_events, 0, sizeof(activities_events));

    struct IDiscordRelationshipEvents relationships_events;
    memset(&relationships_events, 0, sizeof(relationships_events));
    relationships_events.on_refresh = OnRelationshipsRefresh;
}

  users_events := default(TDiscordUserEvents);
  users_events.on_current_user_update := OnUserUpdated;

  activities_events := default(TDiscordActivityEvents);

  relationships_events := default(TDiscordRelationshipEvents);
  relationships_events.on_refresh := OnRelationshipsRefresh;

{
    struct DiscordCreateParams params;
    DiscordCreateParamsSetDefault(&params);
    params.client_id = 418559331265675294;
    params.flags = DiscordCreateFlags_Default;
    params.event_data = &app;
    params.activity_events = &activities_events;
    params.relationship_events = &relationships_events;
    params.user_events = &users_events;
    DISCORD_REQUIRE(DiscordCreate(DISCORD_VERSION, &params, &app.core));
}

  params := default(TDiscordCreateParams);
  DiscordCreateParamsSetDefault(params);
  params.client_id := fApplicationId;
  params.flags := Ord(DiscordCreateFlags_NoRequireDiscord); // or DiscordCreateFlags_Default
  params.event_data := Self;
// Not required per https://github.com/discord/discord-api-docs/issues/4298
//  params.activity_events := @activities_events;
//  params.relationship_events := @relationships_events;
//  params.user_events := @users_events;

  DoLog(Format('SizeOf(NativeUInt) - %d', [SizeOf(NativeUInt)]));
  DoLog(Format('SizeOf(PNativeUInt) - %d', [SizeOf(PNativeUInt)]));
  DoLog(Format('SizeOf(Pointer) - %d', [SizeOf(Pointer)]));
  DoLog(Format('SizeOf(PProc) - %d', [SizeOf(@OnUserUpdated)]));
  DoLog(Format('SizeOf(fDiscordCore^) - %d', [SizeOf(fDiscordCore^)]));
  DoLog(Format('SizeOf(users_events) - %d', [SizeOf(users_events)]));
  DoLog(Format('SizeOf(activities_events) - %d', [SizeOf(activities_events)]));
  DoLog(Format('SizeOf(relationships_events) - %d', [SizeOf(relationships_events)]));
  DoLog(Format('SizeOf(params) - %d', [SizeOf(params)]));
  DoLog(Format('SizeOf(TDiscordActivity) - %d', [SizeOf(TDiscordActivity)]));

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));

  res := fDLLDiscordCreate(DISCORD_VERSION, @params, @fDiscordCore);

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));

  DoLog(Format('DiscordCreate - %s (%d)', [DiscordResultString[res], Ord(res)]));

{
    app.users = app.core->get_user_manager(app.core);
    app.achievements = app.core->get_achievement_manager(app.core);
    app.activities = app.core->get_activity_manager(app.core);
    app.application = app.core->get_application_manager(app.core);
    app.lobbies = app.core->get_lobby_manager(app.core);
}

  fDiscordActivityManager := fDiscordCore.get_activity_manager(fDiscordCore);

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));

  //application := fDiscordCore.get_application_manager(fDiscordCore);

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));

  fDiscordCore.set_log_hook(fDiscordCore, DiscordLogLevel_Debug, Pointer(5), OnLogHook);

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));

  fActive := True;
end;


// Set detail for player activity.
// Player badge in userlist
//
//    Playing %appname%
//
// Player info minicard:
//
//    %appname%
//    aState
//    aDetail
//    00:00 elapsed/left
procedure TDiscord4Delphi.ActivityChange(const aDetail, aState: string; aActivityStart, aActivityEnd: TDateTime);
var
  da: TDiscordActivity;
  I: Integer;
  s: RawByteString;
begin
  Assert(fActive);
  Assert(Length(aDetail) >= 3, 'Needs to be at least 3 char long');
  Assert(Length(aState) >= 3, 'Needs to be at least 3 char long');

  da := default(TDiscordActivity);

  // Unused
  //da.&type := DiscordActivityType_Streaming;

  // Unused
  //da.application_id := 418559331265675294;

  // Unused
  //s := UTF8Encode(aName);
  //for I := 1 to Length(s) do
  //  da.name[I-1] := s[I];

  // Third line
  s := UTF8Encode(aDetail);
  for I := 1 to Length(s) do
    da.state[I-1] := s[I];

  // Second line
  s := UTF8Encode(aState);
  for I := 1 to Length(s) do
    da.details[I-1] := s[I];

  // xx:xx:xx elapsed
  da.timestamps.start := DateTimeToUnix(aActivityStart, False);

  // xx:xx:xx left
  da.timestamps.&end := DateTimeToUnix(aActivityEnd, False);

  // Unused?
  //da.instance := True;

  // Unused?
  //da.supported_platforms := 3;

  fDiscordActivityManager.update_activity(fDiscordActivityManager, @da, Pointer(5), OnActivityCallback);
end;


// Seems to be broken (also see https://github.com/discord/gamesdk-and-dispatch/issues/114)
procedure TDiscord4Delphi.ActivityClear;
begin
  Assert(fActive);

  // OnLogHook - ResponseError
  // { code: InvalidPayload,
  //   message: "child \"activity\" fails because [child \"supported_platforms\" fails because [\"supported_platforms\" must contain at least 1 items]]" }
  // OnActivityCallback - InvalidPayload (5)
  fDiscordActivityManager.clear_activity(fDiscordActivityManager, Pointer(5), OnActivityCallback);
end;


end.
