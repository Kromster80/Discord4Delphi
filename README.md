# Discord4Delphi

Attempt to create MVP integration of Delphi with Discord.

IMPORTANT

Despite Discord Game SDK having `DiscordCreateFlags_NoRequireDiscord`, Discord still needs to be installed and running. Otherwise `DiscordCreate` returns error code `DiscordResult_InternalError = 4`.

- Goal 1 - Link DLL without errors. Done!
- Goal 2 - Change activity ("playing a game" text).
