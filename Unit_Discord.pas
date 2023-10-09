unit Unit_Discord;
interface
uses
  System.SysUtils, System.Variants, System.Classes, Windows,
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
    destructor Destroy; override;

    procedure InitDiscord;
  end;


  TDApplication = record
    core: IDiscordCore;
    users: Pointer;//IDiscordUserManager* users;
    achievements: Pointer;//IDiscordAchievementManager* achievements;
    activities: Pointer;//IDiscordActivityManager* activities;
    relationships: Pointer;//IDiscordRelationshipManager* relationships;
    application: Pointer;//IDiscordApplicationManager* application;
    lobbies: Pointer;//IDiscordLobbyManager* lobbies;
    user_id: TDiscordUserId;
  end;

implementation


{ TDiscord4Delphi }
constructor TDiscord4Delphi.Create(aOnLog: TProc<string>);
begin
  inherited Create;

  fOnLog := aOnLog;

  // Load DLL dynamically, so we could move it into the utility folder
  LoadDLL(DISCORD_GAME_SDK_DLL_NAME);
end;


destructor TDiscord4Delphi.Destroy;
begin

  inherited;
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


procedure TDiscord4Delphi.InitDiscord;
var
  app: TDApplication;
  params: TDiscordCreateParams;
  res: TDiscordResult;
begin
  app := default(TDApplication);
                     {
  struct IDiscordUserEvents users_events;
  memset(&users_events, 0, sizeof(users_events));
  users_events.on_current_user_update = OnUserUpdated;
  struct IDiscordActivityEvents activities_events;
  memset(&activities_events, 0, sizeof(activities_events));
  struct IDiscordRelationshipEvents relationships_events;
  memset(&relationships_events, 0, sizeof(relationships_events));
  relationships_events.on_refresh = OnRelationshipsRefresh;
                                                        }
  params := default(TDiscordCreateParams);
  DiscordCreateParamsSetDefault(@params);
  params.client_id := 418559331265675294;
  params.flags := Ord(DiscordCreateFlags_Default);
  params.event_data := @app;
  params.activity_events := nil;//&activities_events;
  params.relationship_events := nil;//&relationships_events;
  params.user_events := nil;//&users_events;

  res := fDLLDiscordCreate(DISCORD_VERSION, @params, app.core);

  DoLog(Format('DiscordCreate - %s (%d)', [DiscordResultString[res], Ord(res)]));
end;


end.
