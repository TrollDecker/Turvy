Attribute VB_Name = "Globals"
'Stores the states of each available key on a keyboard (whether it is pressed or not).
Global KeyIsDown(255) As Boolean

'Timers.
Global ReportTimer As Integer                               'Dictates the length of time in which failure and success messages are displayed.
Global TickCount As Integer                                 'Used in physics calculations.

'Array into which map data is read from a file.
Global Map(0 To 24, 0 To 17) As Integer

'Level number and the name loaded from the map file.
Global Level As Integer
Global LevelName As String

'Stores the file names and locations of the player's animation frames.
Global PlayerIdle(0 To 3) As String
Global PlayerWalk(0 To 3, 0 To 5) As String
Global PlayerDazed(0 To 3, 0 To 1) As String
Global PlayerShock(0 To 3, 0 To 1) As String

'Player position and gravity, initilally loaded from the map file.
Global PlayerName As String                                 'Player's name for scoreboard purposes. Will be restricted to 3 characters.
Global PlayerPos(0 To 1) As Integer                         'Player's current position. Index 0 is the X coordinate, while 1 is Y.
Global PlayerFallOrigin(0 To 1) As Integer                  'Point from which the player starts falling. Used to determine falling distance.
Global PlayerGravityDir As Integer                          'Direction of the player's current gravity pull.
Global PlayerFallSpeed As Integer                           'Player's fall speed. Gradually increases from zero to 30.
Global PlayerState As Byte                                  'Determines whether the player has been shocked, dazed or has completed the level.
Global PlayerAnim As Byte                                   'Player's current animation frame.
Global PlayerAnimSequence As Byte                           'Determines the playback sequence of the player's animation frames.
Global PlayerAnimLoaded As Boolean                          'Prevents the game from needlessly loading an animation frame that has already been loaded.
Global PlayerRetries As Byte                                'The number of lives the player gets.

'Stores the time taken to complete a level.
Global LevelMinutes As Integer
Global LevelSeconds As Integer
Global LevelMilliseconds As Integer

'Stores the filenames and locations of each robot's animation frames.
Global RobotWalk(0 To 1, 0 To 3, 0 To 3) As String

'Robot positions and gravity, initally loaded from the map file.
Global RobotPos(0 To 1, 0 To 1) As Integer                  'Robot's current position.
Global RobotDir(0 To 1) As Integer                          'Robot's gravitational direction.
Global RobotFacing(0 To 1) As Byte                          'Direction in which the robot faces.
Global RobotAnim(0 To 1) As Integer                         'Robot's current animation frame.
Global RobotAnimLoaded As Boolean                           'Prevents the game from needlessly loading an animation frame that has already been loaded.

'Used in calculations to determine the necessary border for each tile.
Global Border As Integer

'Stores the filenames of each sprite and tile.
Global TileTable(0 To 15) As String
Global ShockTile As String
Global StartTile As String
Global FinishTile As String
