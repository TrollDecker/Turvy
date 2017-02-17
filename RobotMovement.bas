Attribute VB_Name = "RobotMovement"

Public Sub MoveBot()
    
    'Determine the future location of the robots.
    Dim FutureRegion(0 To 1) As Integer
    
    For a = 0 To 1
        
        'If the robot is set to face its relative left...
        If RobotDir(a) = 0 Then
            If RobotFacing(a) = 0 Then
                
                'If the robot's animation frame is not 0 then decrement that value by 1 (moving it closer to 0).
                If RobotAnim(a) > 0 Then
                    RobotAnim(a) = RobotAnim(a) - 1
                    RobotAnimLoaded = False
                    
                'Otherwise...
                Else
                
                    'Determine where the robot's next position will be.
                    FutureRegion(0) = Int((RobotPos(a, 0) + 5 + (8 * (a + 1))) / 32)
                    FutureRegion(1) = Int(RobotPos(a, 1) / 32)
                    
                    'If the robot is going to collide with a solid block or enter a block region with empty space below it,
                    'then place the robot directly adjacent to that block and flip the robot's facing direction.
                    If Map(FutureRegion(0), FutureRegion(1)) >= 1 Or Not Map(FutureRegion(0), FutureRegion(1) - 1) = 1 Then
                        RobotPos(a, 0) = FutureRegion(0) * 32 - (8 * (a + 1))
                        RobotFacing(a) = 1
                        
                    'Otherwise just move the robot along its path.
                    Else
                        RobotPos(a, 0) = RobotPos(a, 0) + 2
                    End If
                    
                End If
                
            'If it's set to face its relative right...
            Else
                
                'If the robot's animation frame is not 3 then increment that value by 1 (moving it closer to 3).
                If RobotAnim(a) < 3 Then
                    RobotAnim(a) = RobotAnim(a) + 1
                    RobotAnimLoaded = False
                    
                'Otherwise...
                Else
                
                    'Determine where the robot's next position will be.
                    FutureRegion(0) = Int((RobotPos(a, 0) - 5 - (8 * (a + 1))) / 32)
                    FutureRegion(1) = Int(RobotPos(a, 1) / 32)
                    
                    'If the robot is going to collide with a solid block or enter a block region with empty space below it,
                    'then place the robot directly adjacent to that block and flip the robot's facing direction.
                    If Map(FutureRegion(0), FutureRegion(1)) >= 1 Or Not Map(FutureRegion(0), FutureRegion(1) - 1) = 1 Then
                        RobotPos(a, 0) = (FutureRegion(0) + 1) * 32 + (8 * (a + 1))
                        RobotFacing(a) = 0
                        
                    'Otherwise just move the robot along its path.
                    Else
                        RobotPos(a, 0) = RobotPos(a, 0) - 2
                    End If
                    
                End If
                
            End If
            
        End If
        
        'If the robot is set to face its relative left...
        If RobotDir(a) = 1 Then
            If RobotFacing(a) = 0 Then
            
                'If the robot's animation frame is not 0 then decrement that value by 1 (moving it closer to 0).
                If RobotAnim(a) > 0 Then
                    RobotAnim(a) = RobotAnim(a) - 1
                    RobotAnimLoaded = False
                
                'Otherwise...
                Else
                
                    'Determine where the robot's next position will be.
                    FutureRegion(0) = Int(RobotPos(a, 0) / 32)
                    FutureRegion(1) = Int((RobotPos(a, 1) + 5 + (8 * (a + 1))) / 32)
                    
                    'If the robot is going to collide with a solid block or enter a block region with empty space below it,
                    'then place the robot directly adjacent to that block and flip the robot's facing direction.
                    If Map(FutureRegion(0), FutureRegion(1)) >= 1 Or Not Map(FutureRegion(0) + 1, FutureRegion(1)) = 1 Then
                        RobotPos(a, 1) = FutureRegion(1) * 32 - (8 * (a + 1))
                        RobotFacing(a) = 1
                    
                    'Otherwise just move the robot along its path.
                    Else
                        RobotPos(a, 1) = RobotPos(a, 1) + 2
                    End If
                    
                End If
            
            'If it's set to face its relative right...
            Else
            
                'If the robot's animation frame is not 3 then increment that value by 1 (moving it closer to 3).
                If RobotAnim(a) < 3 Then
                    RobotAnim(a) = RobotAnim(a) + 1
                    RobotAnimLoaded = False
                    
                'Otherwise...
                Else
                
                    'Determine where the robot's next position will be.
                    FutureRegion(0) = Int(RobotPos(a, 0) / 32)
                    FutureRegion(1) = Int((RobotPos(a, 1) - 5 - (8 * (a + 1))) / 32)
                    
                    'If the robot is going to collide with a solid block or enter a block region with empty space below it,
                    'then place the robot directly adjacent to that block and flip the robot's facing direction.
                    If Map(FutureRegion(0), FutureRegion(1)) >= 1 Or Not Map(FutureRegion(0) + 1, FutureRegion(1)) = 1 Then
                        RobotPos(a, 1) = (FutureRegion(1) + 1) * 32 + (8 * (a + 1))
                        RobotFacing(a) = 0
                        
                    'Otherwise just move the robot along its path.
                    Else
                        RobotPos(a, 1) = RobotPos(a, 1) - 2
                    End If
                    
                End If
                
            End If
        End If
    
        'If the robot is set to face its relative left...
        If RobotDir(a) = 2 Then
            If RobotFacing(a) = 0 Then
            
                'If the robot's animation frame is not 0 then decrement that value by 1 (moving it closer to 0).
                If RobotAnim(a) > 0 Then
                    RobotAnim(a) = RobotAnim(a) - 1
                    RobotAnimLoaded = False
                
                'Otherwise...
                Else
                    
                    'Determine where the robot's next position will be.
                    FutureRegion(0) = Int((RobotPos(a, 0) - 5 - (8 * (a + 1))) / 32)
                    FutureRegion(1) = Int(RobotPos(a, 1) / 32)
                    
                    'If the robot is going to collide with a solid block or enter a block region with empty space below it,
                    'then place the robot directly adjacent to that block and flip the robot's facing direction.
                    If Map(FutureRegion(0), FutureRegion(1)) = 1 Or Not Map(FutureRegion(0), FutureRegion(1) + 1) = 1 Then
                        RobotPos(a, 0) = (FutureRegion(0) + 1) * 32 + (8 * (a + 1))
                        RobotFacing(a) = 1
                        
                    'Otherwise just move the robot along its path.
                    Else
                        RobotPos(a, 0) = RobotPos(a, 0) - 2
                    End If
                    
                End If
                
            'If it's set to face its relative right...
            Else
            
                'If the robot's animation frame is not 3 then increment that value by 1 (moving it closer to 3).
                If RobotAnim(a) < 3 Then
                    RobotAnim(a) = RobotAnim(a) + 1
                    RobotAnimLoaded = False
                    
                'Otherwise...
                Else
                
                    'Determine where the robot's next position will be.
                    FutureRegion(0) = Int((RobotPos(a, 0) + 5 + (8 * (a + 1))) / 32)
                    FutureRegion(1) = Int(RobotPos(a, 1) / 32)
                    
                    'If the robot is going to collide with a solid block or enter a block region with empty space below it,
                    'then place the robot directly adjacent to that block and flip the robot's facing direction.
                    If Map(FutureRegion(0), FutureRegion(1)) = 1 Or Not Map(FutureRegion(0), FutureRegion(1) + 1) = 1 Then
                        RobotPos(a, 0) = (FutureRegion(0)) * 32 - (8 * (a + 1))
                        RobotFacing(a) = 0
                    
                    'Otherwise just move the robot along its path.
                    Else
                        RobotPos(a, 0) = RobotPos(a, 0) + 2
                    End If
                    
                End If
                
            End If
        End If
        
        'If the robot is set to face its relative left...
        If RobotDir(a) = 3 Then
            If RobotFacing(a) = 0 Then
            
                'If the robot's animation frame is not 0 then decrement that value by 1 (moving it closer to 0).
                If RobotAnim(a) > 0 Then
                    RobotAnim(a) = RobotAnim(a) - 1
                    RobotAnimLoaded = False
                
                'Otherwise...
                Else
                    
                    'Determine where the robot's next position will be.
                    FutureRegion(0) = Int(RobotPos(a, 0) / 32)
                    FutureRegion(1) = Int((RobotPos(a, 1) - 5 - (8 * (a + 1))) / 32)
                    
                    'If the robot is going to collide with a solid block or enter a block region with empty space below it,
                    'then place the robot directly adjacent to that block and flip the robot's facing direction.
                    If Map(FutureRegion(0), FutureRegion(1)) = 1 Or Not Map(FutureRegion(0) - 1, FutureRegion(1)) = 1 Then
                        RobotPos(a, 1) = (FutureRegion(1) + 1) * 32 + (8 * (a + 1))
                        RobotFacing(a) = 1
                    
                    'Otherwise just move the robot along its path.
                    Else
                        RobotPos(a, 1) = RobotPos(a, 1) - 2
                    End If
                    
                End If
                
            'If it's set to face its relative right...
            Else
            
                'If the robot's animation frame is not 3 then increment that value by 1 (moving it closer to 3).
                If RobotAnim(a) < 3 Then
                    RobotAnim(a) = RobotAnim(a) + 1
                    RobotAnimLoaded = False
                    
                'Otherwise...
                Else
                
                    'Determine where the robot's next position will be.
                    FutureRegion(0) = Int(RobotPos(a, 0) / 32)
                    FutureRegion(1) = Int((RobotPos(a, 1) + 5 + (8 * (a + 1))) / 32)
                    
                    'If the robot is going to collide with a solid block or enter a block region with empty space below it,
                    'then place the robot directly adjacent to that block and flip the robot's facing direction.
                    If Map(FutureRegion(0), FutureRegion(1)) = 1 Or Not Map(FutureRegion(0) - 1, FutureRegion(1)) = 1 Then
                        RobotPos(a, 1) = FutureRegion(1) * 32 - (8 * (a + 1))
                        RobotFacing(a) = 0
                        
                    'Otherwise just move the robot along its path.
                    Else
                        RobotPos(a, 1) = RobotPos(a, 1) + 2
                    End If
                    
                End If
                
            End If
        End If
        
        'If the current animation frame has not been loaded yet, do so.
        If RobotAnimLoaded = False Then
            frmGravity.imgRobot(a).Picture = LoadPicture(RobotWalk(a, RobotDir(a), RobotAnim(a)))
            RobotAnimLoaded = True
        End If
    Next a
End Sub

Public Sub CheckRobotContact()
    For a = 0 To 1
        'If the difference between the player's position and that of the robot is equal to their half-widths combined and
        'half-heights combined, then shock the player.
        If Abs(PlayerPos(0) - RobotPos(a, 0)) <= frmGravity.imgPlayer.Width / 2 + frmGravity.imgRobot(a).Width / 2 Then
            If Abs(PlayerPos(1) - RobotPos(a, 1)) <= frmGravity.imgPlayer.Height / 2 + frmGravity.imgRobot(a).Height / 2 Then
                Call PlayerStates.ShockPlayer
            End If
        End If
    Next a
End Sub
