Attribute VB_Name = "Levels"
Public Sub LoadLevel(Pack As String)

    'Open the correct level file based on the variables Pack and Level.
    Open "Maps/" & Pack & "_" & Level & ".lvl" For Input As 1
    
    'Read the level name and character positions.
    Input #1, LevelName
    Input #1, PlayerPos(0), PlayerPos(1), PlayerGravityDir
    Input #1, RobotPos(0, 0), RobotPos(0, 1), RobotDir(0), RobotFacing(0)
    Input #1, RobotPos(1, 0), RobotPos(1, 1), RobotDir(1), RobotFacing(1)
    
    'Read the map layout into the Map array.
    For y = 0 To 17
        For x = 0 To 24
            Input #1, Map(x, y)
        Next x
    Next y

    'Close the file.
    Close #1
    
    'Pass the level name to the level name label at the bottom of the screen.
    frmGravity.lblLevelName.Caption = LevelName
    
    'The player and robot positions on file only relates to its grid reference rather than actual pixel position.
    'Convert the player position to pixel position, ensuring she stays in the middle of her starting block.
    PlayerPos(0) = PlayerPos(0) * 32 + 16
    PlayerPos(1) = PlayerPos(1) * 32 + 16
    
    'Set the player's fall origin to their current position.
    PlayerFallOrigin(0) = PlayerPos(0)
    PlayerFallOrigin(1) = PlayerPos(1)
    
    'Set the player object on screen to match their starting position on file.
    frmGravity.imgPlayer.Left = PlayerPos(0) - frmGravity.imgPlayer.Width / 2
    frmGravity.imgPlayer.Top = PlayerPos(1) - frmGravity.imgPlayer.Height / 2
    
    For a = 0 To 1
    
        'Convert the robot positions to pixel positions.
        RobotPos(a, 0) = RobotPos(a, 0) * 32 + 16
        RobotPos(a, 1) = RobotPos(a, 1) * 32 + 16
        
        'Set the robot objects on screen to match their starting positions on file.
        frmGravity.imgRobot(a).Left = RobotPos(a, 0) - frmGravity.imgRobot(a).Width / 2
        frmGravity.imgRobot(a).Top = RobotPos(a, 1) - frmGravity.imgRobot(a).Height / 2

        'Select the correct animation frame based on which direction they are facing.
        If RobotFacing(a) = 0 Then
            RobotAnim(a) = 0
        Else
            RobotAnim(a) = 3
        End If
        
        'Load that animation frame.
        frmGravity.imgRobot(a).Picture = LoadPicture(RobotWalk(a, RobotDir(a), RobotAnim(a)))
        RobotAnimLoaded = True
    Next a
    
    For y = 0 To 17
        For x = 0 To 24
            
            'If the current block is 1 then calculate the correct tile to use, otherwise just reset it to a
            'blank space.
            If Map(x, y) = 1 Then
            
                'Set border to zero. This will be incremented based on the empty space surrounding it, and
                'used to select the correct tile image.
                Border = 0
                
                'Determine the border of each tile by checking for adjacent tiles that are set to empty space.
                'Is there empty space above it?
                If y - 1 >= 0 Then
                    If Not Map(x, y - 1) = 1 Then
                        Border = Border + 1
                    End If
                End If
                
                'To the right?
                If x + 1 <= 24 Then
                    If Not Map(x + 1, y) = 1 Then
                        Border = Border + 2
                    End If
                End If
                
                'Below?
                If y + 1 <= 17 Then
                    If Not Map(x, y + 1) = 1 Then
                        Border = Border + 4
                    End If
                End If
                
                'To the left?
                If x - 1 >= 0 Then
                    If Not Map(x - 1, y) = 1 Then
                        Border = Border + 8
                    End If
                End If
                
                'Load the correct tile image into the current block.
                frmGravity.imgRegion((1 * x) + (25 * y)).Picture = LoadPicture(TileTable(Border))
                
            'If the current block number is 2 then load a shock tile in its place.
            ElseIf Map(x, y) = 2 Then
                frmGravity.imgRegion((1 * x) + (25 * y)).Picture = LoadPicture(ShockTile)
                
            'If the current block number is 3 then load a start/entry tile in its place.
            ElseIf Map(x, y) = 3 Then
                frmGravity.imgRegion((1 * x) + (25 * y)).Picture = LoadPicture(StartTile)
                
            'If the current block number is 3 then load a finish/exit tile in its place.
            ElseIf Map(x, y) = 4 Then
                frmGravity.imgRegion((1 * x) + (25 * y)).Picture = LoadPicture(FinishTile)
                
            'If the current block number is 0 then it is empty space and no image shouls be loaded. Reset the block to empty space.
            Else
                frmGravity.imgRegion((1 * x) + (25 * y)).Picture = LoadPicture("")

            End If
            
            'Position the tile at its appropriate coordinates on the form.
            frmGravity.imgRegion((1 * x) + (25 * y)).Left = 32 * x
            frmGravity.imgRegion((1 * x) + (25 * y)).Top = 32 * y
        Next x
    Next y
      
    'Load the idle image appropriate to their gravitational direction.
    frmGravity.imgPlayer.Picture = LoadPicture(PlayerIdle(PlayerGravityDir))
    
    'Reset the player's current state, walk animation frame, playback sequence and the timer for success/fail reports.
    PlayerStates.ResetPlayerState
    PlayerAnim = 3
    PlayerAnimSequence = 1
    ReportTimer = 0
End Sub
