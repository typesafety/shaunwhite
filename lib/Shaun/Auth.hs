{- |
TODO: Allow for more granularity (i.e. more control than "is admin? yes/no").
-}
module Shaun.Auth
       ( isAdminOf
       , authAndRunWithMsg
       ) where

import Relude

import Data.Bits (testBit)
import Discord (restCall, DiscordHandler, RestCallErrorCode)
import Discord.Types (Message (..), Guild (..), GuildMember (..), Role (..), User (..))
import Discord.Requests (GuildRequest (GetGuildMember, GetGuild))

import Shaun.Env (Shaun)


-- | Check if a member of a Guild is an admin of that Guild.
isAdminOf :: GuildMember -> Guild -> Bool
isAdminOf member guild =
    let mRoleIds = memberRoles member
        gRoles = guildRoles guild
        mRoles = filter (\ role -> roleId role `elem` mRoleIds) gRoles
    in any roleIsAdmin mRoles
  where
    -- Discord permissions are calculated using bitwise operations, see:
    -- https://discord.com/developers/docs/topics/permissions#permissions-bitwise-permission-flags
    roleIsAdmin :: Role -> Bool
    roleIsAdmin = (`testBit` adminBit) . rolePerms
      where
        adminBit :: Int
        adminBit = 3  -- 0x00000008, (1 << 3)

authAndRunWithMsg
    :: Message  -- ^ Message, from which to determine whether to run the command or not.
    -> a        -- ^ Default value to if command was not authorized.
    -> Shaun DiscordHandler (Either RestCallErrorCode a)
                -- ^ Command to run if authorization was successful.
    -> Shaun DiscordHandler (Either RestCallErrorCode a)
authAndRunWithMsg msg defaultValue cmdToRun = do
    Just gId <- pure $ messageGuild msg
    Right guild <- lift $ restCall (GetGuild gId)
    Right gMember <- lift $ restCall (GetGuildMember gId $ (userId . messageAuthor) msg)

    if gMember `isAdminOf` guild
        then cmdToRun
        else pure . Right $ defaultValue
