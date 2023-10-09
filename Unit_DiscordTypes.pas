unit Unit_DiscordTypes;
interface
uses
  System.SysUtils, System.Variants, System.Classes;

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
  TDiscordVersion = Int32;
  TDiscordClientId = Int64;
  TDiscordSnowflake = Int64;
  TDiscordUserId = TDiscordSnowflake;
{struct DiscordCreateParams {
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

  TDiscordCreateParams = record
    Client_id: TDiscordClientId;
    Flags: UInt64;
    Events: Pointer;
    Event_data: Pointer;
    Application_events: Pointer;
    Application_version: TDiscordVersion;
    User_events: Pointer;
    User_version: TDiscordVersion;
    Image_events: Pointer;
    Image_version: TDiscordVersion;
    Activity_events: Pointer;
    Activity_version: TDiscordVersion;
    Relationship_events: Pointer;
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

{struct IDiscordCore {
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
  IDiscordCore = record
    {void (DISCORD_API *destroy)(struct IDiscordCore* core);
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
    struct IDiscordAchievementManager* (DISCORD_API *get_achievement_manager)(struct IDiscordCore* core);   }
  end;
  // enum EDiscordResult DISCORD_API DiscordCreate(DiscordVersion version, struct DiscordCreateParams* params, struct IDiscordCore** result);

  TDLLDiscordCreate = function(aVersion: TDiscordVersion; aParams: PDiscordCreateParams; aResult: IDiscordCore): TDiscordResult; cdecl;

  procedure DiscordCreateParamsSetDefault(aParams: PDiscordCreateParams);

implementation


procedure DiscordCreateParamsSetDefault(aParams: PDiscordCreateParams);
begin
  aParams^ := default(TDiscordCreateParams);
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
