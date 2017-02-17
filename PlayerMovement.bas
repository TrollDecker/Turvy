Attribute VB_Name = "PlayerMovement"
'Increases the velocity of the player's fall.
Public Function IncreaseVelocity(u As Integer, g As Integer, t As Integer, DimensionScaler As Single)
    IncreaseVelocity = (u * t + 0.5 * g * t * t) * DimensionScaler
End Function

Public Sub MovePlayer()
    
    'Used to determine where the player's position in be next, and whether it will place them in collision with a solid tile.
    Dim FutureRegion(0 To 1) As Integer
    
    'Sets the current animation frame to their idle stance.
    frmGravity.imgPlayer.Picture = LoadPicture(PlayerIdle(PlayerGravityDir))
    
    If PlayerFallSpeed = 0 Then
        'If the character's gravity direction is up or down, then she can only move left or right.
        If PlayerGravityDir = 0 Or PlayerGravityDir = 2 Then
        
            'If D is pressed...
            If KeyIsDown(vbKeyD) = True Then
            
                'Determine where the player's right edge will be.
                FutureRegion(0) = Int((PlayerPos(0) + 5 + 8) / 32)
                FutureRegion(1) = Int(PlayerPos(1) / 32)
                
                'If the player's right edge will land within a solid tile, place the player exactly adjacent to the block instead.
                If Map(FutureRegion(0), FutureRegion(1)) >= 1 Then
                    PlayerPos(0) = FutureRegion(0) * 32 - 8
                    
                    'If that tile is a shock tile, shock the player.
                    If Map(FutureRegion(0), FutureRegion(1)) = 2 Then
                        Call PlayerStates.ShockPlayer
                    End If
                    
                    'If that tile is an exit tile, the player has completed the level.
                    If Map(FutureRegion(0), FutureRegion(1)) = 4 Then
                        Call PlayerStates.WinLevel
                    End If
                    
                'If the player's right edge is not going to collide with a solid tile, simply move them along.
                Else
                    PlayerPos(0) = PlayerPos(0) + 5
                End If
                
                'Ensure the player animation only displays the right-walking frames.
                If PlayerAnim < 3 Then
                    PlayerAnim = 3
                    PlayerAnimSequence = 1
                    PlayerAnimLoaded = False
                End If
                
                'Alternate between animation sequences when those sequences reach their first or last frames.
                If PlayerAnimSequence = 0 Then
                    PlayerAnim = PlayerAnim - 1
                    If PlayerAnim = 3 Then
                        PlayerAnimSequence = 0
                    End If
                    PlayerAnimLoaded = False
                End If
                
                If PlayerAnimSequence = 1 Then
                    PlayerAnim = PlayerAnim + 1
                    If PlayerAnim = 5 Then
                        PlayerAnimSequence = 0
                    End If
                    PlayerAnimLoaded = False
                End If
                
            End If
            
            'If A is pressed...
            If KeyIsDown(vbKeyA) = True Then
            
                'Determine where the player's left edge will be.
                FutureRegion(0) = Int((PlayerPos(0) - 5 - 8) / 32)
                FutureRegion(1) = Int(PlayerPos(1) / 32)

                'If the player's left edge will land within a solid tile, place the player exactly adjacent to the block instead.
                If Map(FutureRegion(0), FutureRegion(1)) >= 1 Then
                    PlayerPos(0) = (FutureRegion(0) + 1) * 32 + 8
                    
                    'If that tile is a shock tile, shock the player.
                    If Map(FutureRegion(0), FutureRegion(1)) = 2 Then
                        Call PlayerStates.ShockPlayer
                    End If
                    
                    'If that tile is an exit tile, the player has completed the level.
                    If Map(FutureRegion(0), FutureRegion(1)) = 4 Then
                        Call PlayerStates.WinLevel
                    End If
                    
                'If the player's left edge is not going to collide with a solid tile, simply move them along.
                Else
                    PlayerPos(0) = PlayerPos(0) - 5
                End If
                
                'Ensure the player animation only displays the left-walking frames.
                If PlayerAnim > 2 Then
                    PlayerAnim = 2
                    PlayerAnimSequence = 1
                    PlayerAnimLoaded = False
                End If
                
                'Alternate between animation sequences when those sequences reach their first or last frames.
                If PlayerAnimSequence = 0 Then
                    PlayerAnim = PlayerAnim + 1
                    If PlayerAnim = 2 Then
                        PlayerAnimSequence = 0
                    End If
                    PlayerAnimLoaded = False
                End If
                
                If PlayerAnimSequence = 1 Then
                    PlayerAnim = PlayerAnim - 1
                    If PlayerAnim = 0 Then
                        PlayerAnimSequence = 0
                    End If
                    PlayerAnimLoaded = False
                End If
                
            End If
        End If
        
        'If the character's gravity direction is left or right, then she can only move up or down.
        If PlayerGravityDir = 1 Or PlayerGravityDir = 3 Then
        
            'If S is pressed...
            If KeyIsDown(vbKeyS) = True Then
            
                'Determine where the player's bottom edge will be.
                FutureRegion(0) = Int(PlayerPos(0) / 32)
                FutureRegion(1) = Int((PlayerPos(1) + 5 + 8) / 32)
                
                'If the player's bottom edge will land within a solid tile, place the player exactly adjacent to the block instead.
                If Map(FutureRegion(0), FutureRegion(1)) >= 1 Then
                    PlayerPos(1) = FutureRegion(1) * 32 - 8
                    
                    'If that tile is a shock tile, shock the player.
                    If Map(FutureRegion(0), FutureRegion(1)) = 2 Then
                        Call PlayerStates.ShockPlayer
                    End If
                    
                    'If that tile is an exit tile, the player has completed the level.
                    If Map(FutureRegion(0), FutureRegion(1)) = 4 Then
                        Call PlayerStates.WinLevel
                    End If
                    
                'If the player's bottom edge is not going to collide with a solid tile, simply move them along.
                Else
                    PlayerPos(1) = PlayerPos(1) + 5
                End If
                
                'Ensure the player animation only displays the downward-walking frames.
                If PlayerAnim < 3 Then
                    PlayerAnim = 3
                    PlayerAnimSequence = 1
                    PlayerAnimLoaded = False
                End If
                
                'Alternate between animation sequences when those sequences reach their first or last frames.
                If PlayerAnimSequence = 0 Then
                    PlayerAnim = PlayerAnim - 1
                    If PlayerAnim = 3 Then
                        PlayerAnimSequence = 0
                    End If
                    PlayerAnimLoaded = False
                End If
                
                If PlayerAnimSequence = 1 Then
                    PlayerAnim = PlayerAnim + 1
                    If PlayerAnim = 5 Then
                        PlayerAnimSequence = 0
                    End If
                    PlayerAnimLoaded = False
                End If
            End If
            
            'If W is pressed...
            If KeyIsDown(vbKeyW) = True Then
                
                'Determine where the player's top edge will be.
                FutureRegion(0) = Int(PlayerPos(0) / 32)
                FutureRegion(1) = Int((PlayerPos(1) - 5 - 8) / 32)
                
                'If the player's top edge will land within a solid tile, place the player exactly adjacent to the block instead.
                If Map(FutureRegion(0), FutureRegion(1)) >= 1 Then
                    PlayerPos(1) = (FutureRegion(1) + 1) * 32 + 8
                    
                    'If that tile is a shock tile, shock the player.
                    If Map(FutureRegion(0), FutureRegion(1)) = 2 Then
                        Call PlayerStates.ShockPlayer
                    End If
                    
                    'If that tile is an exit tile, the player has completed the level.
                    If Map(FutureRegion(0), FutureRegion(1)) = 4 Then
                        Call PlayerStates.WinLevel
                    End If
                
                'If the player's top edge is not going to collide with a solid tile, simply move them along.
                Else
                    PlayerPos(1) = PlayerPos(1) - 5
                End If
                
                'Ensure the player animation only displays the upward-walking frames.
                If PlayerAnim > 2 Then
                    PlayerAnim = 2
                    PlayerAnimSequence = 1
                    PlayerAnimLoaded = False
                End If
                                
                'Alternate between animation sequences when those sequences reach their first or last frames.
                If PlayerAnimSequence = 0 Then
                    PlayerAnim = PlayerAnim + 1
                    If PlayerAnim = 2 Then
                        PlayerAnimSequence = 0
                    End If
                    PlayerAnimLoaded = False
                End If
                
                If PlayerAnimSequence = 1 Then
                    PlayerAnim = PlayerAnim - 1
                    If PlayerAnim = 0 Then
                        PlayerAnimSequence = 0
                    End If
                    PlayerAnimLoaded = False
                End If
            End If
        End If
        
        'If an arrow key is pressed, the player's gravity direction changes, their fall origin is reset to their
        'position at the time of the change and a sprite image corresponding to that direction is loaded.
        If KeyIsDown(vbKeyUp) = True Then
            PlayerGravityDir = 0
            PlayerFallOrigin(0) = PlayerPos(0)
            PlayerFallOrigin(1) = PlayerPos(1)
            frmGravity.imgPlayer.Picture = LoadPicture(PlayerIdle(0))
        End If
        
        If KeyIsDown(vbKeyRight) = True Then
            PlayerGravityDir = 1
            PlayerFallOrigin(0) = PlayerPos(0)
            PlayerFallOrigin(1) = PlayerPos(1)
            frmGravity.imgPlayer.Picture = LoadPicture(PlayerIdle(1))
        End If
        
        If KeyIsDown(vbKeyDown) = True Then
            PlayerGravityDir = 2
            PlayerFallOrigin(0) = PlayerPos(0)
            PlayerFallOrigin(1) = PlayerPos(1)
            frmGravity.imgPlayer.Picture = LoadPicture(PlayerIdle(2))
        End If
        
        If KeyIsDown(vbKeyLeft) = True Then
            PlayerGravityDir = 3
            PlayerFallOrigin(0) = PlayerPos(0)
            PlayerFallOrigin(1) = PlayerPos(1)
            frmGravity.imgPlayer.Picture = LoadPicture(PlayerIdle(3))
        End If
        
    End If
    
    'If the current animation frame has not been loaded, then do so.
    If PlayerAnimLoaded = False Then
        frmGravity.imgPlayer.Picture = LoadPicture(PlayerWalk(PlayerGravityDir, PlayerAnim))
        PlayerAnimLoaded = True
    End If
    
End Sub

Public Sub DropPlayer()

    'Used to determine where the player's position in be next, and whether it will place them in collision with a solid tile.
    Dim FutureRegion(0 To 1, 0 To 1) As Integer
    
    'If the player's gravity is up...
    If PlayerGravityDir = 0 Then
        
        'Determine the future location of the player's top side, and their left and right sides.
        FutureRegion(0, 0) = Int((PlayerPos(0) - 8) / 32)
        FutureRegion(0, 1) = Int((PlayerPos(0) + 7) / 32)
        FutureRegion(1, 0) = Int((PlayerPos(1) - PlayerFallSpeed - 17) / 32)
        
        'If the top edge collides with a solid block (by checking both the left and right ends of that side)...
        If FutureRegion(1, 0) >= 0 Then
            If Map(FutureRegion(0, 0), FutureRegion(1, 0)) >= 1 Or Map(FutureRegion(0, 1), FutureRegion(1, 0)) >= 1 Then
            
                'Position the player exactly adjactent to that block and reset TickCount.
                PlayerPos(1) = (FutureRegion(1, 0) + 1) * 32 + 16
                TickCount = 0
                
                'If the player has fallen more than two blocks, daze them.
                If Abs(PlayerPos(1) - PlayerFallOrigin(1)) > 80 Then
                    Call PlayerStates.DazePlayer
                End If
                
                'If the block is a shock tile, shock the player.
                If Map(FutureRegion(0, 0), FutureRegion(1, 0)) = 2 Or Map(FutureRegion(0, 1), FutureRegion(1, 0)) = 2 Then
                    Call PlayerStates.ShockPlayer
                End If
                
                'If the block is an exit gate, the player has completed the level.
                If Map(FutureRegion(0, 0), FutureRegion(1, 0)) = 4 Or Map(FutureRegion(0, 1), FutureRegion(1, 0)) = 4 Then
                    Call PlayerStates.WinLevel
                End If
                
                'Reset the player's fall speed and fall origin.
                PlayerFallSpeed = 0
                PlayerFallOrigin(0) = PlayerPos(0)
                PlayerFallOrigin(1) = PlayerPos(1)
                
            'Otherwise just drop the player by the value of their fall speed.
            Else
                PlayerPos(1) = Int(PlayerPos(1) - PlayerFallSpeed)
            End If
            
        End If
    End If
    
    'If the player's gravity is right...
    If PlayerGravityDir = 1 Then
    
        'Determine the future location of the player's right side, and their top and bottom sides.
        FutureRegion(0, 0) = Int((PlayerPos(0) + PlayerFallSpeed + 16) / 32)
        FutureRegion(1, 0) = Int((PlayerPos(1) - 8) / 32)
        FutureRegion(1, 1) = Int((PlayerPos(1) + 7) / 32)
        
        'If the right edge collides with a solid block (by checking both the top and bottom ends of that side)...
        If FutureRegion(0, 0) <= 24 Then
            If Map(FutureRegion(0, 0), FutureRegion(1, 0)) >= 1 Or Map(FutureRegion(0, 0), FutureRegion(1, 1)) >= 1 Then
            
                'Position the player exactly adjactent to that block and reset TickCount.
                PlayerPos(0) = FutureRegion(0, 0) * 32 - 16
                TickCount = 0
                
                'If the player has fallen more than two blocks, daze them.
                If Abs(PlayerPos(0) - PlayerFallOrigin(0)) > 80 Then
                    Call PlayerStates.DazePlayer
                End If
                
                'If the block is a shock tile, shock the player.
                If Map(FutureRegion(0, 0), FutureRegion(1, 0)) = 2 Or Map(FutureRegion(0, 0), FutureRegion(1, 1)) = 2 Then
                    Call PlayerStates.ShockPlayer
                End If
                
                'If the block is an exit gate, the player has completed the level.
                If Map(FutureRegion(0, 0), FutureRegion(1, 0)) = 4 Or Map(FutureRegion(0, 0), FutureRegion(1, 1)) = 4 Then
                    Call PlayerStates.WinLevel
                End If
                
                'Reset the player's fall speed and fall origin.
                PlayerFallSpeed = 0
                PlayerFallOrigin(0) = PlayerPos(0)
                PlayerFallOrigin(1) = PlayerPos(1)

            'Otherwise just drop the player by the value of their fall speed.
            Else
                PlayerPos(0) = Int(PlayerPos(0) + PlayerFallSpeed)
            End If
            
        End If
    End If
    
    If PlayerGravityDir = 2 Then
    
        'Determine the future location of the player's bottom side, and their left and right sides.
        FutureRegion(0, 0) = Int((PlayerPos(0) - 8) / 32)
        FutureRegion(0, 1) = Int((PlayerPos(0) + 7) / 32)
        FutureRegion(1, 0) = Int((PlayerPos(1) + PlayerFallSpeed + 16) / 32)
        
        'If the bottom edge collides with a solid block (by checking both the left and right ends of that side)...
        If FutureRegion(1, 0) < 18 Then
            If Map(FutureRegion(0, 0), FutureRegion(1, 0)) >= 1 Or Map(FutureRegion(0, 1), FutureRegion(1, 0)) >= 1 Then
                
                'Position the player exactly adjactent to that block and reset TickCount.
                PlayerPos(1) = FutureRegion(1, 0) * 32 - 16
                TickCount = 0
                
                'If the player has fallen more than two blocks, daze them.
                If Abs(PlayerPos(1) - PlayerFallOrigin(1)) > 80 Then
                    Call PlayerStates.DazePlayer
                End If
                
                'If the block is a shock tile, shock the player.
                If Map(FutureRegion(0, 0), FutureRegion(1, 0)) = 2 Or Map(FutureRegion(0, 1), FutureRegion(1, 0)) = 2 Then
                    Call PlayerStates.ShockPlayer
                End If
                
                'If the block is an exit gate, the player has completed the level.
                If Map(FutureRegion(0, 0), FutureRegion(1, 0)) = 4 Or Map(FutureRegion(0, 1), FutureRegion(1, 0)) = 4 Then
                    Call PlayerStates.WinLevel
                End If
                
                'Reset the player's fall speed and fall origin.
                PlayerFallSpeed = 0
                PlayerFallOrigin(0) = PlayerPos(0)
                PlayerFallOrigin(1) = PlayerPos(1)
                
            'Otherwise just drop the player by the value of their fall speed.
            Else
                PlayerPos(1) = Int(PlayerPos(1) + PlayerFallSpeed)
            End If
            
        End If
    End If
    
    If PlayerGravityDir = 3 Then
    
        'Determine the future location of the player's left side, and their top and bottom sides.
        FutureRegion(0, 0) = Int((PlayerPos(0) - PlayerFallSpeed - 17) / 32)
        FutureRegion(1, 0) = Int((PlayerPos(1) - 8) / 32)
        FutureRegion(1, 1) = Int((PlayerPos(1) + 7) / 32)
        
        'If the left edge collides with a solid block (by checking both the top and bottom ends of that side)...
        If FutureRegion(0, 0) >= 0 Then
            If Map(FutureRegion(0, 0), FutureRegion(1, 0)) >= 1 Or Map(FutureRegion(0, 0), FutureRegion(1, 1)) >= 1 Then
            
                'Position the player exactly adjactent to that block and reset TickCount.
                PlayerPos(0) = (FutureRegion(0, 0) + 1) * 32 + 16
                TickCount = 0
                
                'If the player has fallen more than two blocks, daze them.
                If Abs(PlayerPos(0) - PlayerFallOrigin(0)) > 80 Then
                    Call PlayerStates.DazePlayer
                End If
                
                'If the block is a shock tile, shock the player.
                If Map(FutureRegion(0, 0), FutureRegion(1, 0)) = 2 Or Map(FutureRegion(0, 0), FutureRegion(1, 1)) = 2 Then
                    Call PlayerStates.ShockPlayer
                End If
                
                'Reset the player's fall speed and fall origin.
                If Map(FutureRegion(0, 0), FutureRegion(1, 0)) = 4 Or Map(FutureRegion(0, 0), FutureRegion(1, 1)) = 4 Then
                    Call PlayerStates.WinLevel
                End If
                
                 'Reset the player's fall speed and fall origin.
                PlayerFallSpeed = 0
                PlayerFallOrigin(0) = PlayerPos(0)
                PlayerFallOrigin(1) = PlayerPos(1)
                
            'Otherwise just drop the player by the value of their fall speed.
            Else
                PlayerPos(0) = Int(PlayerPos(0) - PlayerFallSpeed)
            End If
            
        End If
    End If
    
    'Various variables displayed for debugging purposes.
    'DebugText = ""
    'DebugText = DebugText & "Player X: " & PlayerPos(0)
    'DebugText = DebugText & Chr(13) & "Player Y: " & PlayerPos(1)
    'DebugText = DebugText & Chr(13) & "Gravity: " & PlayerGravityDir
    'DebugText = DebugText & Chr(13) & "Fall Dist X: " & PlayerPos(0) - PlayerFallOrigin(0)
    'DebugText = DebugText & Chr(13) & "Fall Dist X: " & PlayerPos(1) - PlayerFallOrigin(1)
    'DebugText = DebugText & Chr(13) & "Region X: " & Int(PlayerPos(0) / 32)
    'DebugText = DebugText & Chr(13) & "Future X: " & FutureRegion(0, 0)
    'DebugText = DebugText & Chr(13) & "Region Y: " & Int(PlayerPos(1) / 32)
    'DebugText = DebugText & Chr(13) & "Future Y: " & FutureRegion(1, 0)
    'DebugText = DebugText & Chr(13) & "Player State: " & PlayerState
    'DebugText = DebugText & Chr(13) & Len(Str(LevelMinutes)) & "  " & LevelMinutes
    'DebugText = DebugText & Chr(13) & Len(Str(LevelSeconds))
    'DebugText = DebugText & Chr(13) & Len(Str(LevelMilliseconds))
    
    'frmDebug.lblDebug.Caption = DebugText
End Sub
