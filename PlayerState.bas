Attribute VB_Name = "PlayerStates"
'Player state for when the player comes into contact with a robot or shock tile.
Public Sub ShockPlayer()
    PlayerState = 3
End Sub

'Player state for when the player falls too far.
Public Sub DazePlayer()
    PlayerState = 2
End Sub

'Player state for when the player successfully reaches the exit.
Public Sub WinLevel()
    PlayerState = 1
End Sub

'Resets the player state to normal.
Public Sub ResetPlayerState()
    PlayerState = 0
End Sub
