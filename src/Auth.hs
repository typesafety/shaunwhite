{- |
TODO: Allow for more granularity (i.e. more control than "is admin? yes/no").
-}
module Auth
       ( isAdminOf
       , authAndRunWithMsg
       ) where

import Data.Bits ((.&.))
import Discord
import Discord.Types
import Discord.Requests

import qualified Env

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
    roleIsAdmin = (adminBit ==) . (adminBit .&.) . rolePerms
      where
        adminBit :: Integer
        adminBit = 0x00000008

authAndRunWithMsg
    :: Message  -- ^ Message, from which to determine whether to run the command or not.
    -> a        -- ^ Default value to if command was not authorized.
    -> Env.Shaun DiscordHandler (Either RestCallErrorCode a)
                -- ^ Command to run if authorization was successful.
    -> Env.Shaun DiscordHandler (Either RestCallErrorCode a)
authAndRunWithMsg msg defaultValue cmdToRun = do
    Just gId <- pure $ messageGuild msg
    Right guild <- lift $ restCall (GetGuild gId)
    Right gMember <- lift $ restCall (GetGuildMember gId $ (userId . messageAuthor) msg)

    if gMember `isAdminOf` guild
        then cmdToRun
        else pure . Right $ defaultValue
