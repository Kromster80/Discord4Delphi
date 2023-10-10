unit Unit_Discord;
interface
uses
  SysUtils, Windows,
  Unit_DiscordTypes;

type
  {
  struct Application {
    struct IDiscordCore* core;
    struct IDiscordUserManager* users;
    struct IDiscordAchievementManager* achievements;
    struct IDiscordActivityManager* activities;
    struct IDiscordRelationshipManager* relationships;
    struct IDiscordApplicationManager* application;
    struct IDiscordLobbyManager* lobbies;
    DiscordUserId user_id;
  }

  TDApplication = record
    core: PDiscordCore;
    users: Pointer;
    achievements: Pointer;
    activities: PDiscordActivityManager;
    relationships: Pointer;
    application: PDiscordApplicationManager;
    lobbies: Pointer;
    user_id: TDiscordUserId;
  end;

  TDiscord4Delphi = class
  private const
    DISCORD_GAME_SDK_DLL_NAME = 'discord_game_sdk 3.2.1.dll';
  private
    fOnLog: TProc<string>;

    fLibHandle: NativeUInt;
    fDLLDiscordCreate: TDLLDiscordCreate;

    fActive: Boolean;
    fClientId: Int64;
    app: TDApplication;

    procedure LoadDLL(const aDLLPath: string);
    procedure DoLog(const aText: string);
  public
    constructor Create(aOnLog: TProc<string>);

    procedure InitDiscord(aClientId: Int64);
    procedure ActivityChange(const aName, aDetail, aState: string);
    procedure ActivityClear;

    procedure Callbacks;
  end;

  procedure OnRelationshipsRefresh(aEvent_data: Pointer); stdcall;
  procedure OnUserUpdated(aEvent_data: Pointer); stdcall;
  procedure OnLogHook(aHook_data: Pointer; aLevel: TDiscordLogLevel; aMessage: PUTF8Char); stdcall;
  procedure OnActivityCallback(aCallback_data: Pointer; aResult: TDiscordResult); stdcall;


implementation


{ TDiscord4Delphi }
constructor TDiscord4Delphi.Create(aOnLog: TProc<string>);
begin
  inherited Create;

  fOnLog := aOnLog;

  // Load DLL dynamically, so we could move it into the utility folder
  LoadDLL(DISCORD_GAME_SDK_DLL_NAME);
end;


procedure TDiscord4Delphi.DoLog(const aText: string);
begin
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


procedure TDiscord4Delphi.Callbacks;
var
  res: TDiscordResult;
begin
  if not fActive then Exit;

  res := app.core.run_callbacks(app.core);

  DoLog(Format('app.core.run_callbacks - %s (%d)', [DiscordResultString[res], Ord(res)]));
end;


procedure OnRelationshipsRefresh(aEvent_data: Pointer);
begin
  sleep(5);
end;


procedure OnUserUpdated(aEvent_data: Pointer);
begin
  sleep(5);
end;


procedure OnLogHook(aHook_data: Pointer; aLevel: TDiscordLogLevel; aMessage: PUTF8Char);
begin
  sleep(5);
end;


procedure OnActivityCallback(aCallback_data: Pointer; aResult: TDiscordResult);
begin
  sleep(5);
end;


procedure TDiscord4Delphi.InitDiscord(aClientId: Int64);
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

  fClientId := aClientId;

  app := default(TDApplication);

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
  params.client_id := fClientId;
  params.flags := Ord(DiscordCreateFlags_NoRequireDiscord); // or DiscordCreateFlags_Default
  params.event_data := @app;
// Not required per https://github.com/discord/discord-api-docs/issues/4298
//  params.activity_events := @activities_events;
//  params.relationship_events := @relationships_events;
//  params.user_events := @users_events;

  DoLog(Format('SizeOf(NativeUInt) - %d', [SizeOf(NativeUInt)]));
  DoLog(Format('SizeOf(PNativeUInt) - %d', [SizeOf(PNativeUInt)]));
  DoLog(Format('SizeOf(Pointer) - %d', [SizeOf(Pointer)]));
  DoLog(Format('SizeOf(PProc) - %d', [SizeOf(@OnUserUpdated)]));
  DoLog(Format('SizeOf(app) - %d', [SizeOf(app)]));
  DoLog(Format('SizeOf(app.core^) - %d', [SizeOf(app.core^)]));
  DoLog(Format('SizeOf(users_events) - %d', [SizeOf(users_events)]));
  DoLog(Format('SizeOf(activities_events) - %d', [SizeOf(activities_events)]));
  DoLog(Format('SizeOf(relationships_events) - %d', [SizeOf(relationships_events)]));
  DoLog(Format('SizeOf(params) - %d', [SizeOf(params)]));
  DoLog(Format('SizeOf(TDiscordActivity) - %d', [SizeOf(TDiscordActivity)]));

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));

  res := fDLLDiscordCreate(DISCORD_VERSION, @params, @app.core);

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

  app.activities := app.core.get_activity_manager(app.core);

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));

  //app.application := app.core.get_application_manager(app.core);

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));

  app.core.set_log_hook(app.core, DiscordLogLevel_Debug, Pointer(5), OnLogHook);

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));

  fActive := True;

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));
end;


procedure TDiscord4Delphi.ActivityChange(const aName, aDetail, aState: string);
var
  da: TDiscordActivity;
  I: Integer;
  s: RawByteString;
begin
  Assert(Length(aDetail) >= 3);
  Assert(Length(aState) >= 3);

  da := default(TDiscordActivity);
  da.&type := DiscordActivityType_Streaming;
  //da.application_id := fClientId;

  s := UTF8Encode(aName);
  for I := 1 to Length(s) do
    da.name[I-1] := s[I];

  // Goes below
  s := UTF8Encode(aDetail);
  for I := 1 to Length(s) do
    da.state[I-1] := s[I];

  // Goes on top
  s := UTF8Encode(aState);
  for I := 1 to Length(s) do
    da.details[I-1] := s[I];

//  da.timestamps.start := 0;
//  da.timestamps.&end := 9999999;
//  da.instance := True;
//  da.supported_platforms := 0;

  app.activities.update_activity(app.activities, @da, Pointer(5), OnActivityCallback);
end;


procedure TDiscord4Delphi.ActivityClear;
begin
  app.activities.clear_activity(app.activities, Pointer(5), OnActivityCallback);
end;


end.
