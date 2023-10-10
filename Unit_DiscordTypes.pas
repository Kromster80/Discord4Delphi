unit Unit_DiscordTypes;
interface

const
  DISCORD_VERSION = 3;
  DISCORD_APPLICATION_MANAGER_VERSION = 1;
  DISCORD_USER_MANAGER_VERSION = 1;
  DISCORD_IMAGE_MANAGER_VERSION = 1;
  DISCORD_ACTIVITY_MANAGER_VERSION = 1;
  DISCORD_RELATIONSHIP_MANAGER_VERSION = 1;
  DISCORD_LOBBY_MANAGER_VERSION = 1;
  DISCORD_NETWORK_MANAGER_VERSION = 1;
  DISCORD_OVERLAY_MANAGER_VERSION = 2;
  DISCORD_STORAGE_MANAGER_VERSION = 1;
  DISCORD_STORE_MANAGER_VERSION = 1;
  DISCORD_VOICE_MANAGER_VERSION = 1;
  DISCORD_ACHIEVEMENT_MANAGER_VERSION = 1;

type
  TDiscordResult = (
    DiscordResult_Ok = 0,
    DiscordResult_ServiceUnavailable = 1,
    DiscordResult_InvalidVersion = 2,
    DiscordResult_LockFailed = 3,
    DiscordResult_InternalError = 4,
    DiscordResult_InvalidPayload = 5,
    DiscordResult_InvalidCommand = 6,
    DiscordResult_InvalidPermissions = 7,
    DiscordResult_NotFetched = 8,
    DiscordResult_NotFound = 9,
    DiscordResult_Conflict = 10,
    DiscordResult_InvalidSecret = 11,
    DiscordResult_InvalidJoinSecret = 12,
    DiscordResult_NoEligibleActivity = 13,
    DiscordResult_InvalidInvite = 14,
    DiscordResult_NotAuthenticated = 15,
    DiscordResult_InvalidAccessToken = 16,
    DiscordResult_ApplicationMismatch = 17,
    DiscordResult_InvalidDataUrl = 18,
    DiscordResult_InvalidBase64 = 19,
    DiscordResult_NotFiltered = 20,
    DiscordResult_LobbyFull = 21,
    DiscordResult_InvalidLobbySecret = 22,
    DiscordResult_InvalidFilename = 23,
    DiscordResult_InvalidFileSize = 24,
    DiscordResult_InvalidEntitlement = 25,
    DiscordResult_NotInstalled = 26,
    DiscordResult_NotRunning = 27,
    DiscordResult_InsufficientBuffer = 28,
    DiscordResult_PurchaseCanceled = 29,
    DiscordResult_InvalidGuild = 30,
    DiscordResult_InvalidEvent = 31,
    DiscordResult_InvalidChannel = 32,
    DiscordResult_InvalidOrigin = 33,
    DiscordResult_RateLimited = 34,
    DiscordResult_OAuth2Error = 35,
    DiscordResult_SelectChannelTimeout = 36,
    DiscordResult_GetGuildTimeout = 37,
    DiscordResult_SelectVoiceForceRequired = 38,
    DiscordResult_CaptureShortcutAlreadyListening = 39,
    DiscordResult_UnauthorizedForAchievement = 40,
    DiscordResult_InvalidGiftCode = 41,
    DiscordResult_PurchaseError = 42,
    DiscordResult_TransactionAborted = 43,
    DiscordResult_DrawingInitFailed = 44
  );

const
  DiscordResultString: array [TDiscordResult] of string = (
    'Ok',
    'ServiceUnavailable',
    'InvalidVersion',
    'LockFailed',
    'InternalError',
    'InvalidPayload',
    'InvalidCommand',
    'InvalidPermissions',
    'NotFetched',
    'NotFound',
    'Conflict',
    'InvalidSecret',
    'InvalidJoinSecret',
    'NoEligibleActivity',
    'InvalidInvite',
    'NotAuthenticated',
    'InvalidAccessToken',
    'ApplicationMismatch',
    'InvalidDataUrl',
    'InvalidBase64',
    'NotFiltered',
    'LobbyFull',
    'InvalidLobbySecret',
    'InvalidFilename',
    'InvalidFileSize',
    'InvalidEntitlement',
    'NotInstalled',
    'NotRunning',
    'InsufficientBuffer',
    'PurchaseCanceled',
    'InvalidGuild',
    'InvalidEvent',
    'InvalidChannel',
    'InvalidOrigin',
    'RateLimited',
    'OAuth2Error',
    'SelectChannelTimeout',
    'GetGuildTimeout',
    'SelectVoiceForceRequired',
    'CaptureShortcutAlreadyListening',
    'UnauthorizedForAchievement',
    'InvalidGiftCode',
    'PurchaseError',
    'TransactionAborted',
    'DrawingInitFailed'
  );

type
  TDiscordCreateFlags = (
    DiscordCreateFlags_Default = 0,
    DiscordCreateFlags_NoRequireDiscord = 1
  );

  TDiscordLogLevel = (
    DiscordLogLevel_Error = 1,
    DiscordLogLevel_Warn,
    DiscordLogLevel_Info,
    DiscordLogLevel_Debug
  );

  TDiscordActivityPartyPrivacy = (
    DiscordActivityPartyPrivacy_Private = 0,
    DiscordActivityPartyPrivacy_Public = 1
  );

  TDiscordActivityType = (
    DiscordActivityType_Playing,
    DiscordActivityType_Streaming,
    DiscordActivityType_Listening,
    DiscordActivityType_Watching
  );

  TDiscordActivityActionType = (
    DiscordActivityActionType_Join = 1,
    DiscordActivityActionType_Spectate
  );

  TDiscordStatus = (
    DiscordStatus_Offline = 0,
    DiscordStatus_Online = 1,
    DiscordStatus_Idle = 2,
    DiscordStatus_DoNotDisturb = 3
  );

  TDiscordRelationshipType = (
    DiscordRelationshipType_None,
    DiscordRelationshipType_Friend,
    DiscordRelationshipType_Blocked,
    DiscordRelationshipType_PendingIncoming,
    DiscordRelationshipType_PendingOutgoing,
    DiscordRelationshipType_Implicit
  );

{
typedef int64_t DiscordClientId;
typedef int32_t DiscordVersion;
typedef int64_t DiscordSnowflake;
typedef int64_t DiscordTimestamp;
typedef DiscordSnowflake DiscordUserId;
}

  TDiscordClientId = Int64;
  TDiscordVersion = Int32;
  TDiscordSnowflake = Int64;
  TDiscordTimestamp = Int64;
  TDiscordUserId = TDiscordSnowflake;

{struct DiscordUser {
    DiscordUserId id;
    char username[256];
    char discriminator[8];
    char avatar[128];
    bool bot;
}

  TDiscordUser = record
    id: TDiscordUserId;
    username: array [0..255] of UTF8Char;
    discriminator: array [0..7] of UTF8Char;
    avatar: array [0..127] of UTF8Char;
    bot: Boolean;
  end;
  PDiscordUser = ^TDiscordUser;

  TDiscordActivityTimestamps = record
    start: TDiscordTimestamp;
    &end: TDiscordTimestamp;
  end;

  TDiscordActivityAssets = record
    large_image: array [0..127] of UTF8Char;
    large_text: array [0..127] of UTF8Char;
    small_image: array [0..127] of UTF8Char;
    small_text: array [0..127] of UTF8Char;
  end;

  TDiscordPartySize = record
    current_size: Int32;
    max_size: Int32;
  end;

{struct DiscordActivityParty {
    char id[128];
    struct DiscordPartySize size;
    enum EDiscordActivityPartyPrivacy privacy;
}

  TDiscordActivityParty = record
    id: array [0..127] of UTF8Char;
    size: TDiscordPartySize;
    privacy: TDiscordActivityPartyPrivacy;
  end;

{struct DiscordActivitySecrets {
    char match[128];
    char join[128];
    char spectate[128];
}

  TDiscordActivitySecrets = record
    match: array [0..127] of UTF8Char;
    join: array [0..127] of UTF8Char;
    spectate: array [0..127] of UTF8Char;
  end;

{struct DiscordActivity {
    enum EDiscordActivityType type;
    int64_t application_id;
    char name[128];
    char state[128];
    char details[128];
    struct DiscordActivityTimestamps timestamps;
    struct DiscordActivityAssets assets;
    struct DiscordActivityParty party;
    struct DiscordActivitySecrets secrets;
    bool instance;
    uint32_t supported_platforms;
}

  TDiscordActivity = record
    &type: TDiscordActivityType;
    application_id: Int64;
    name: array [0..127] of UTF8Char;
    state: array [0..127] of UTF8Char;
    details: array [0..127] of UTF8Char;
    timestamps: TDiscordActivityTimestamps;
    assets: TDiscordActivityAssets;
    party: TDiscordActivityParty;
    secrets: TDiscordActivitySecrets;
    instance: Boolean;
    supported_platforms: UInt32;
  end;
  PDiscordActivity = ^TDiscordActivity;

{
struct DiscordPresence {
    enum EDiscordStatus status;
    struct DiscordActivity activity;
}

  TDiscordPresence = record
    status: TDiscordStatus;
    activity: TDiscordActivity;
  end;

{
struct DiscordRelationship {
    enum EDiscordRelationshipType type;
    struct DiscordUser user;
    struct DiscordPresence presence;
}

  TDiscordRelationship = record
    &type: TDiscordRelationshipType;
    user: TDiscordUser;
    presence: TDiscordPresence;
  end;
  PDiscordRelationship = ^TDiscordRelationship;

{
struct IDiscordApplicationManager {
    void (DISCORD_API *validate_or_exit)(struct IDiscordApplicationManager* manager, void* callback_data, void (DISCORD_API *callback)(void* callback_data, enum EDiscordResult result));
    void (DISCORD_API *get_current_locale)(struct IDiscordApplicationManager* manager, DiscordLocale* locale);
    void (DISCORD_API *get_current_branch)(struct IDiscordApplicationManager* manager, DiscordBranch* branch);
    void (DISCORD_API *get_oauth2_token)(struct IDiscordApplicationManager* manager, void* callback_data, void (DISCORD_API *callback)(void* callback_data, enum EDiscordResult result, struct DiscordOAuth2Token* oauth2_token));
    void (DISCORD_API *get_ticket)(struct IDiscordApplicationManager* manager, void* callback_data, void (DISCORD_API *callback)(void* callback_data, enum EDiscordResult result, const char* data));
}

  PDiscordApplicationManager = ^TDiscordApplicationManager;
  TDiscordCallback = procedure(aCallback_data: Pointer; aResult: TDiscordResult); stdcall;
  TDiscordvalidate_or_exit = procedure(aManager: PDiscordApplicationManager; callback_data: Pointer; callback: TDiscordCallback); stdcall;
//  TDiscordget_current_locale = procedure(aManager: PDiscordApplicationManager; aLocale: PDiscordLocale); stdcall;
//  TDiscordget_current_branch = procedure(aManager: PDiscordApplicationManager; aBranch: PDiscordBranch); stdcall;
//  TDiscordget_oauth2_token = procedure(aManager: PDiscordApplicationManager; void* callback_data, void (DISCORD_API *callback)(void* callback_data, enum EDiscordResult result, struct DiscordOAuth2Token* oauth2_token)); stdcall;
//  TDiscordget_ticket = procedure(aManager: PDiscordApplicationManager; void* callback_data, void (DISCORD_API *callback)(void* callback_data, enum EDiscordResult result, const char* data)); stdcall;
  TDiscordApplicationManager = record
    validate_or_exit: TDiscordvalidate_or_exit;
    get_current_locale: Pointer;//todo: TDiscordget_current_locale;
    get_current_branch: Pointer;//todo: TDiscordget_current_branch;
    get_oauth2_token: Pointer;//todo: TDiscordget_oauth2_token;
    get_ticket: Pointer;//todo: TDiscordget_ticket;
  end;

{
struct IDiscordUserEvents {
    void (DISCORD_API *on_current_user_update)(void* event_data);
}
  TDiscordEventData = procedure(aEvent_data: Pointer); stdcall;
  TDiscordUserEvents = record
    On_current_user_update: TDiscordEventData;
  end;

{
struct IDiscordActivityEvents {
    void (DISCORD_API *on_activity_join)(void* event_data, const char* secret);
    void (DISCORD_API *on_activity_spectate)(void* event_data, const char* secret);
    void (DISCORD_API *on_activity_join_request)(void* event_data, struct DiscordUser* user);
    void (DISCORD_API *on_activity_invite)(void* event_data, enum EDiscordActivityActionType type, struct DiscordUser* user, struct DiscordActivity* activity);
}

  TDiscordOn_activity_join = procedure(aEvent_data: Pointer; aSecret: PUTF8Char); stdcall;
  TDiscordOn_activity_spectate = procedure(aEvent_data: Pointer; aSecret: PUTF8Char); stdcall;
  TDiscordOn_activity_join_request = procedure(aEvent_data: Pointer; aUser: PDiscordUser); stdcall;
  TDiscordOn_activity_invite = procedure(aEvent_data: Pointer; aType: TDiscordActivityActionType; aUser: PDiscordUser; aActivity: PDiscordActivity); stdcall;
  TDiscordActivityEvents = record
    On_activity_join: TDiscordOn_activity_join;
    On_activity_spectate: TDiscordOn_activity_spectate;
    On_activity_join_request: TDiscordOn_activity_join_request;
    On_activity_invite: TDiscordOn_activity_invite;
  end;
  PDiscordActivityEvents = ^TDiscordActivityEvents;

{
struct IDiscordActivityManager {
    enum EDiscordResult (DISCORD_API *register_command)(struct IDiscordActivityManager* manager, const char* command);
    enum EDiscordResult (DISCORD_API *register_steam)(struct IDiscordActivityManager* manager, uint32_t steam_id);
    void (DISCORD_API *update_activity)(struct IDiscordActivityManager* manager, struct DiscordActivity* activity, void* callback_data, void (DISCORD_API *callback)(void* callback_data, enum EDiscordResult result));
    void (DISCORD_API *clear_activity)(struct IDiscordActivityManager* manager, void* callback_data, void (DISCORD_API *callback)(void* callback_data, enum EDiscordResult result));
    void (DISCORD_API *send_request_reply)(struct IDiscordActivityManager* manager, DiscordUserId user_id, enum EDiscordActivityJoinRequestReply reply, void* callback_data, void (DISCORD_API *callback)(void* callback_data, enum EDiscordResult result));
    void (DISCORD_API *send_invite)(struct IDiscordActivityManager* manager, DiscordUserId user_id, enum EDiscordActivityActionType type, const char* content, void* callback_data, void (DISCORD_API *callback)(void* callback_data, enum EDiscordResult result));
    void (DISCORD_API *accept_invite)(struct IDiscordActivityManager* manager, DiscordUserId user_id, void* callback_data, void (DISCORD_API *callback)(void* callback_data, enum EDiscordResult result));
}
  PDiscordActivityManager = ^TDiscordActivityManager;
  TDiscordActivityManagerregister_command = function(aManager: PDiscordActivityManager; aCommand: PUTF8Char): TDiscordResult; stdcall;
  TDiscordActivityManagerCallback = procedure(aCallback_data: Pointer; aResult: TDiscordResult); stdcall;
  TDiscordActivityManagerupdate_activity = procedure(aManager: PDiscordActivityManager; aActivity: PDiscordActivity; aCallback_data: Pointer; aCallback: TDiscordActivityManagerCallback); stdcall;
  TDiscordActivityManagerclear_activity = procedure(aManager: PDiscordActivityManager; aCallback_data: Pointer; aCallback: TDiscordActivityManagerCallback); stdcall;
  TDiscordActivityManager = record
    register_command: TDiscordActivityManagerregister_command;
    register_steam: Pointer;
    update_activity: TDiscordActivityManagerupdate_activity;
    clear_activity: TDiscordActivityManagerclear_activity;
    send_request_reply: Pointer;
    send_invite: Pointer;
    accept_invite: Pointer;
  end;

{
struct IDiscordRelationshipEvents {
    void (DISCORD_API *on_refresh)(void* event_data);
    void (DISCORD_API *on_relationship_update)(void* event_data, struct DiscordRelationship* relationship);
}

  TDiscordon_relationship_update = procedure(aEvent_data: Pointer; aRelationship: PDiscordRelationship); stdcall;
  TDiscordRelationshipEvents = record
    on_refresh: TDiscordEventData;
    on_relationship_update: TDiscordon_relationship_update;
  end;
  PDiscordRelationshipEvents = ^TDiscordRelationshipEvents;

{
struct IDiscordCore {
    void (DISCORD_API *destroy)(struct IDiscordCore* core);
    enum EDiscordResult (DISCORD_API *run_callbacks)(struct IDiscordCore* core);
    void (DISCORD_API *set_log_hook)(struct IDiscordCore* core, enum EDiscordLogLevel min_level, void* hook_data, void (DISCORD_API *hook)(void* hook_data, enum EDiscordLogLevel level, const char* message));
    struct IDiscordApplicationManager* (DISCORD_API *get_application_manager)(struct IDiscordCore* core);
    struct IDiscordUserManager* (DISCORD_API *get_user_manager)(struct IDiscordCore* core);
    struct IDiscordImageManager* (DISCORD_API *get_image_manager)(struct IDiscordCore* core);
    struct IDiscordActivityManager* (DISCORD_API *get_activity_manager)(struct IDiscordCore* core);
    struct IDiscordRelationshipManager* (DISCORD_API *get_relationship_manager)(struct IDiscordCore* core);
    struct IDiscordLobbyManager* (DISCORD_API *get_lobby_manager)(struct IDiscordCore* core);
    struct IDiscordNetworkManager* (DISCORD_API *get_network_manager)(struct IDiscordCore* core);
    struct IDiscordOverlayManager* (DISCORD_API *get_overlay_manager)(struct IDiscordCore* core);
    struct IDiscordStorageManager* (DISCORD_API *get_storage_manager)(struct IDiscordCore* core);
    struct IDiscordStoreManager* (DISCORD_API *get_store_manager)(struct IDiscordCore* core);
    struct IDiscordVoiceManager* (DISCORD_API *get_voice_manager)(struct IDiscordCore* core);
    struct IDiscordAchievementManager* (DISCORD_API *get_achievement_manager)(struct IDiscordCore* core);
}


  PDiscordCore = ^TDiscordCore;
  PPDiscordCore = ^PDiscordCore;
  TDiscordCoreDestroy = procedure(aCore: PDiscordCore); stdcall;
  TDiscordCoreRun_callbacks = function(aCore: PDiscordCore): TDiscordResult; stdcall;
  TDiscordLogHook = procedure(aHook_data: Pointer; aLevel: TDiscordLogLevel; aMessage: PUTF8Char); stdcall;
  TDiscordCoreSet_log_hook = procedure(aCore: PDiscordCore; aMin_level: TDiscordLogLevel; aHook_data: Pointer; aHook: TDiscordLogHook); stdcall;
  TDiscordCoreGet_application_manager = function(aCore: PDiscordCore): PDiscordApplicationManager; stdcall;
  TDiscordCoreGet_user_manager = function(aCore: PDiscordCore): Pointer; stdcall;
  TDiscordCoreGet_image_manager = function(aCore: PDiscordCore): Pointer; stdcall;
  TDiscordCoreGet_activity_manager = function(aCore: PDiscordCore): PDiscordActivityManager; stdcall;
  TDiscordCoreGet_relationship_manager = function(aCore: PDiscordCore): Pointer; stdcall;
  TDiscordCoreGet_lobby_manager = function(aCore: PDiscordCore): Pointer; stdcall;
  TDiscordCoreGet_network_manager = function(aCore: PDiscordCore): Pointer; stdcall;
  TDiscordCoreGet_overlay_manager = function(aCore: PDiscordCore): Pointer; stdcall;
  TDiscordCoreGet_storage_manager = function(aCore: PDiscordCore): Pointer; stdcall;
  TDiscordCoreGet_store_manager = function(aCore: PDiscordCore): Pointer; stdcall;
  TDiscordCoreGet_voice_manager = function(aCore: PDiscordCore): Pointer; stdcall;
  TDiscordCoreGet_achievement_manager = function(aCore: PDiscordCore): Pointer; stdcall;

  TDiscordCore = record {60}
    Destroy: TDiscordCoreDestroy;
    run_callbacks: TDiscordCoreRun_callbacks;
    set_log_hook: TDiscordCoreSet_log_hook;
    get_application_manager: TDiscordCoreGet_application_manager;
    get_user_manager: TDiscordCoreGet_user_manager;
    get_image_manager: TDiscordCoreGet_image_manager;
    get_activity_manager: TDiscordCoreGet_activity_manager;
    get_relationship_manager: TDiscordCoreGet_relationship_manager;
    get_lobby_manager: TDiscordCoreGet_lobby_manager;
    get_network_manager: TDiscordCoreGet_network_manager;
    get_overlay_manager: TDiscordCoreGet_overlay_manager;
    get_storage_manager: TDiscordCoreGet_storage_manager;
    get_store_manager: TDiscordCoreGet_store_manager;
    get_voice_manager: TDiscordCoreGet_voice_manager;
    get_achievement_manager: TDiscordCoreGet_achievement_manager;
  end;

{
typedef void* IDiscordCoreEvents;
}

  TDiscordCoreEvents = Pointer;
  PDiscordCoreEvents = ^TDiscordCoreEvents;

{
struct DiscordCreateParams {
    DiscordClientId client_id;
    uint64_t flags;
    IDiscordCoreEvents* events;
    void* event_data;
    IDiscordApplicationEvents* application_events;
    DiscordVersion application_version;
    struct IDiscordUserEvents* user_events;
    DiscordVersion user_version;
    IDiscordImageEvents* image_events;
    DiscordVersion image_version;
    struct IDiscordActivityEvents* activity_events;
    DiscordVersion activity_version;
    struct IDiscordRelationshipEvents* relationship_events;
    DiscordVersion relationship_version;
    struct IDiscordLobbyEvents* lobby_events;
    DiscordVersion lobby_version;
    struct IDiscordNetworkEvents* network_events;
    DiscordVersion network_version;
    struct IDiscordOverlayEvents* overlay_events;
    DiscordVersion overlay_version;
    IDiscordStorageEvents* storage_events;
    DiscordVersion storage_version;
    struct IDiscordStoreEvents* store_events;
    DiscordVersion store_version;
    struct IDiscordVoiceEvents* voice_events;
    DiscordVersion voice_version;
    struct IDiscordAchievementEvents* achievement_events;
    DiscordVersion achievement_version;
}

  TDiscordCreateParams = record {120 = 2 * 8 + 26 * 4}
    Client_id: TDiscordClientId;
    Flags: UInt64;
    Events: PDiscordCoreEvents;
    Event_data: Pointer;
    Application_events: Pointer;
    Application_version: TDiscordVersion;
    User_events: Pointer;
    User_version: TDiscordVersion;
    Image_events: Pointer;
    Image_version: TDiscordVersion;
    Activity_events: PDiscordActivityEvents;
    Activity_version: TDiscordVersion;
    Relationship_events: PDiscordRelationshipEvents;
    Relationship_version: TDiscordVersion;
    Lobby_events: Pointer;
    Lobby_version: TDiscordVersion;
    Network_events: Pointer;
    Network_version: TDiscordVersion;
    Overlay_events: Pointer;
    Overlay_version: TDiscordVersion;
    Storage_events: Pointer;
    Storage_version: TDiscordVersion;
    Store_events: Pointer;
    Store_version: TDiscordVersion;
    Voice_events: Pointer;
    Voice_version: TDiscordVersion;
    Achievement_events: Pointer;
    Achievement_version: TDiscordVersion;
  end;
  PDiscordCreateParams = ^TDiscordCreateParams;


  // enum EDiscordResult DISCORD_API DiscordCreate(DiscordVersion version, struct DiscordCreateParams* params, struct IDiscordCore** result);
  TDLLDiscordCreate = function(aVersion: TDiscordVersion; aParams: PDiscordCreateParams; aResult: PPDiscordCore): TDiscordResult; stdcall;

  // void DiscordCreateParamsSetDefault(struct DiscordCreateParams* params)
  procedure DiscordCreateParamsSetDefault(var aParams: TDiscordCreateParams);

  //function DiscordCreate(aVersion: TDiscordVersion; aParams: PDiscordCreateParams; aResult: PPDiscordCore): TDiscordResult; stdcall; external 'discord_game_sdk 2.5.6.dll';

implementation


procedure DiscordCreateParamsSetDefault(var aParams: TDiscordCreateParams);
begin
  aParams := default(TDiscordCreateParams);
  aParams.application_version := DISCORD_APPLICATION_MANAGER_VERSION;
  aParams.user_version := DISCORD_USER_MANAGER_VERSION;
  aParams.image_version := DISCORD_IMAGE_MANAGER_VERSION;
  aParams.activity_version := DISCORD_ACTIVITY_MANAGER_VERSION;
  aParams.relationship_version := DISCORD_RELATIONSHIP_MANAGER_VERSION;
  aParams.lobby_version := DISCORD_LOBBY_MANAGER_VERSION;
  aParams.network_version := DISCORD_NETWORK_MANAGER_VERSION;
  aParams.overlay_version := DISCORD_OVERLAY_MANAGER_VERSION;
  aParams.storage_version := DISCORD_STORAGE_MANAGER_VERSION;
  aParams.store_version := DISCORD_STORE_MANAGER_VERSION;
  aParams.voice_version := DISCORD_VOICE_MANAGER_VERSION;
  aParams.achievement_version := DISCORD_ACHIEVEMENT_MANAGER_VERSION;
end;


end.
