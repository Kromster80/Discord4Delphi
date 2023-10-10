unit Unit_Discord;
interface
uses
  SysUtils, Windows,
  Unit_DiscordTypes;

type
  TDiscord4Delphi = class
  private const
    DISCORD_GAME_SDK_DLL_NAME = 'discord_game_sdk.dll';
  private
    fOnLog: TProc<string>;

    fLibHandle: NativeUInt;
    fDLLDiscordCreate: TDLLDiscordCreate;
    procedure LoadDLL(const aDLLPath: string);
    procedure DoLog(const aText: string);
  public
    constructor Create(aOnLog: TProc<string>);

    procedure InitDiscord;
  end;

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
    activities: Pointer;
    relationships: Pointer;
    application: PDiscordApplicationManager;
    lobbies: Pointer;
    user_id: TDiscordUserId;
  end;

  procedure OnRelationshipsRefresh(aEvent_data: Pointer); stdcall;
  procedure OnUserUpdated(aEvent_data: Pointer); stdcall;


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


procedure OnRelationshipsRefresh(aEvent_data: Pointer);
begin
  Assert(False, 'OnRelationshipsRefresh');
end;


procedure OnUserUpdated(aEvent_data: Pointer);
begin
  Assert(False, 'OnUserUpdated');
end;


procedure TDiscord4Delphi.InitDiscord;
var
  app: TDApplication;
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
  params.client_id := 418559331265675294;
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

  res := fDLLDiscordCreate(DISCORD_VERSION, @params, @app.core);

  DoLog(Format('DiscordCreate - %s (%d)', [DiscordResultString[res], Ord(res)]));

{
    app.users = app.core->get_user_manager(app.core);
    app.achievements = app.core->get_achievement_manager(app.core);
    app.activities = app.core->get_activity_manager(app.core);
    app.application = app.core->get_application_manager(app.core);
    app.lobbies = app.core->get_lobby_manager(app.core);
}

  res := app.core.run_callbacks(app.core);

  DoLog(Format('app.core.run_callbacks - %s (%d)', [DiscordResultString[res], Ord(res)]));

  err := GetLastError;
  if err <> 0 then
    raise Exception.Create(Format('GetLastError - %d', [err]));
end;


end.
