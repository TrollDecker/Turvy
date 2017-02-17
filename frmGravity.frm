VERSION 5.00
Begin VB.Form frmGravity 
   BackColor       =   &H00FFFFFF&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Turvy"
   ClientHeight    =   9000
   ClientLeft      =   1350
   ClientTop       =   1650
   ClientWidth     =   12000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   600
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   800
   Begin VB.TextBox txtPlayerName 
      Alignment       =   2  'Center
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   5700
      MaxLength       =   3
      TabIndex        =   8
      Top             =   6825
      Width           =   600
   End
   Begin VB.PictureBox picScores 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   1500
      Left            =   3000
      ScaleHeight     =   1470
      ScaleWidth      =   5970
      TabIndex        =   7
      Top             =   3000
      Visible         =   0   'False
      Width           =   6000
   End
   Begin VB.CommandButton cmdTopScores 
      Caption         =   "Top Scores"
      Height          =   450
      Left            =   6000
      TabIndex        =   6
      Top             =   7725
      Width           =   1500
   End
   Begin VB.PictureBox picHelp 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   6600
      Left            =   3000
      ScaleHeight     =   6570
      ScaleWidth      =   5970
      TabIndex        =   5
      Top             =   150
      Visible         =   0   'False
      Width           =   6000
   End
   Begin VB.CommandButton cmdHelp 
      Caption         =   "How to Play"
      Height          =   450
      Left            =   4500
      TabIndex        =   4
      Top             =   7725
      Width           =   1500
   End
   Begin VB.CommandButton cmdStart 
      Caption         =   "Start"
      Height          =   450
      Left            =   4500
      TabIndex        =   3
      Top             =   7200
      Width           =   3000
   End
   Begin VB.Timer tmrTimer 
      Enabled         =   0   'False
      Left            =   11520
      Top             =   8520
   End
   Begin VB.Image imgRobot 
      Height          =   480
      Index           =   1
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRobot 
      Height          =   480
      Index           =   0
      Left            =   480
      Top             =   8520
      Width           =   480
   End
   Begin VB.Label lblTimer 
      Alignment       =   2  'Center
      Caption         =   "Timer: 00:00:00"
      Height          =   360
      Left            =   9120
      TabIndex        =   2
      Top             =   8640
      Width           =   2880
   End
   Begin VB.Label lblLevelName 
      Alignment       =   2  'Center
      Height          =   360
      Left            =   2880
      TabIndex        =   1
      Top             =   8640
      Width           =   6240
   End
   Begin VB.Image imgPlayer 
      Height          =   480
      Left            =   0
      Top             =   8520
      Width           =   480
   End
   Begin VB.Label lblRetries 
      Alignment       =   2  'Center
      Caption         =   "Retries: ?"
      Height          =   360
      Left            =   0
      TabIndex        =   0
      Top             =   8640
      Width           =   2880
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   449
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   448
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   447
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   446
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   445
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   444
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   443
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   442
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   441
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   440
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   439
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   438
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   437
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   436
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   435
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   434
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   433
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   432
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   431
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   430
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   429
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   428
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   427
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   426
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   425
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   424
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   423
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   422
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   421
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   420
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   419
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   418
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   417
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   416
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   415
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   414
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   413
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   412
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   411
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   410
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   409
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   408
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   407
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   406
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   405
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   404
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   403
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   402
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   401
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   400
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   399
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   398
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   397
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   396
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   395
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   394
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   393
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   392
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   391
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   390
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   389
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   388
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   387
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   386
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   385
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   384
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   383
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   382
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   381
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   380
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   379
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   378
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   377
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   376
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   375
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   374
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   373
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   372
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   371
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   370
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   369
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   368
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   367
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   366
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   365
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   364
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   363
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   362
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   361
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   360
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   359
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   358
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   357
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   356
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   355
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   354
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   353
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   352
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   351
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   350
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   349
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   348
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   347
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   346
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   345
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   344
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   343
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   342
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   341
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   340
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   339
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   338
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   337
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   336
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   335
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   334
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   333
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   332
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   331
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   330
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   329
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   328
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   327
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   326
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   325
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   324
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   323
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   322
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   321
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   320
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   319
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   318
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   317
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   316
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   315
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   314
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   313
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   312
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   311
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   310
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   309
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   308
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   307
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   306
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   305
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   304
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   303
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   302
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   301
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   300
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   299
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   298
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   297
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   296
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   295
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   294
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   293
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   292
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   291
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   290
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   289
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   288
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   287
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   286
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   285
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   284
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   283
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   282
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   281
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   280
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   279
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   278
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   277
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   276
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   275
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   274
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   273
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   272
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   271
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   270
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   269
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   268
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   267
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   266
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   265
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   264
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   263
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   262
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   261
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   260
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   259
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   258
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   257
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   256
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   255
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   254
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   253
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   252
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   251
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   250
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   249
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   248
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   247
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   246
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   245
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   244
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   243
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   242
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   241
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   240
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   239
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   238
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   237
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   236
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   235
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   234
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   233
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   232
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   231
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   230
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   229
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   228
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   227
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   226
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   225
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   224
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   223
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   222
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   221
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   220
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   219
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   218
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   217
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   216
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   215
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   214
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   213
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   212
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   211
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   210
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   209
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   208
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   207
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   206
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   205
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   204
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   203
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   202
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   201
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   200
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   199
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   198
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   197
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   196
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   195
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   194
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   193
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   192
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   191
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   190
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   189
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   188
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   187
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   186
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   185
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   184
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   183
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   182
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   181
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   180
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   179
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   178
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   177
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   176
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   175
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   174
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   173
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   172
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   171
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   170
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   169
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   168
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   167
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   166
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   165
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   164
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   163
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   162
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   161
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   160
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   159
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   158
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   157
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   156
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   155
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   154
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   153
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   152
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   151
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   150
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   149
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   148
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   147
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   146
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   145
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   144
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   143
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   142
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   141
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   140
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   139
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   138
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   137
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   136
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   135
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   134
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   133
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   132
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   131
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   130
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   129
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   128
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   127
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   126
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   125
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   124
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   123
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   122
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   121
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   120
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   119
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   118
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   117
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   116
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   115
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   114
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   113
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   112
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   111
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   110
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   109
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   108
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   107
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   106
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   105
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   104
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   103
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   102
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   101
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   100
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   99
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   98
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   97
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   96
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   95
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   94
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   93
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   92
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   91
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   90
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   89
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   88
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   87
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   86
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   85
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   84
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   83
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   82
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   81
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   80
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   79
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   78
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   77
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   76
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   75
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   74
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   73
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   72
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   71
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   70
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   69
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   68
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   67
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   66
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   65
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   64
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   63
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   62
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   61
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   60
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   59
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   58
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   57
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   56
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   55
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   54
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   53
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   52
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   51
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   50
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   49
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   48
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   47
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   46
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   45
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   44
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   43
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   42
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   41
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   40
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   39
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   38
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   37
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   36
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   35
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   34
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   33
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   32
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   31
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   30
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   29
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   28
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   27
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   26
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   25
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   24
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   23
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   22
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   21
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   20
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   19
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   18
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   17
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   16
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   15
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   14
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   13
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   12
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   11
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   10
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   9
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   8
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   7
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   6
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   5
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   4
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   3
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   2
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   1
      Left            =   0
      Top             =   0
      Width           =   480
   End
   Begin VB.Image imgRegion 
      Height          =   480
      Index           =   0
      Left            =   0
      Top             =   0
      Width           =   480
   End
End
Attribute VB_Name = "frmGravity"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdHelp_Click()
    Dim HelpLine As String
    If picHelp.Visible = False Then
        picScores.Visible = False
        picHelp.Visible = True
        
        picHelp.Cls
        
        Open "Help.txt" For Input As #1
            Do While Not EOF(1)
                Input #1, HelpLine
                
                picHelp.Print HelpLine
            Loop
        Close #1
    Else
        picHelp.Visible = False
    End If
End Sub

Private Sub cmdStart_Click()
    PlayerName = UCase(txtPlayerName.Text)
    
    If Not PlayerName = "" Then

        'Sets the timer away and hides all title screen elements.
        tmrTimer.Enabled = True
        
        picHelp.Visible = False
        picScores.Visible = False
        cmdHelp.Visible = False
        cmdTopScores.Visible = False
        txtPlayerName.Visible = False
        cmdStart.Visible = False
        
        'Loads the first level.
        Call ResetClock
        Call LoadLevel("Lab")
        
        'Resets the number of lives and passes it to the retries label.
        PlayerRetries = 3
        lblRetries.Caption = "Retries: " & PlayerRetries
        
    End If
End Sub

Private Sub cmdTopScores_Click()
    Dim TopName As String
    Dim TopTime As String
    Dim TopMinute, TopSecond, TopMillisecond As Integer
        
    If picScores.Visible = False Then
        picHelp.Visible = False
        picScores.Visible = True
        picScores.Cls
        
        picScores.Print "===THE FASTEST TIMES==="
        picScores.Print ""
        
        Open "Scores.txt" For Input As #1
        
        Do While Not EOF(1)
            Input #1, TopName, TopMinute, TopSecond, TopMillisecond
            
            TopTime = ""
            If TopMinute < 10 Then
                TopTime = TopTime & "0" & TopMinute & ":"
            Else
                TopTime = TopTime & TopMinute & ":"
            End If
            
            If TopSecond < 10 Then
                TopTime = TopTime & "0" & TopSecond & ":"
            Else
                TopTime = TopTime & TopSecond & ":"
            End If
            
            If TopMillisecond < 10 Then
                TopTime = TopTime & "0" & TopMillisecond
            Else
                TopTime = TopTime & TopMillisecond
            End If
            
            picScores.Print TopName, TopTime
        Loop
        
        Close #1
    Else
        picScores.Visible = False
    End If
End Sub

Private Sub Form_Load()
    'frmDebug.Show
    
    'Stores the necessary file names for each Character's Animation.
    For GravDir = 0 To 3
        'Player's idle stance.
        PlayerIdle(GravDir) = "Sprites/Character_Idle_" & GravDir & ".gif"
        
        'Player's walking animation.
        For PlayerWalkFrame = 0 To 5
            PlayerWalk(GravDir, PlayerWalkFrame) = "Sprites/Character_Walk_" & GravDir & "_" & PlayerWalkFrame & ".gif"
        Next PlayerWalkFrame
        
        'Player's other animations.
        For PlayerOtherFrame = 0 To 1
            PlayerShock(GravDir, PlayerOtherFrame) = "Sprites/Character_Shock_" & GravDir & "_" & PlayerOtherFrame & ".gif"
            PlayerDazed(GravDir, PlayerOtherFrame) = "Sprites/Character_Dazed_" & GravDir & "_" & PlayerOtherFrame & ".gif"
        Next PlayerOtherFrame
        
        'Robot animations.
        For RobotFrame = 0 To 3
            For Robot = 0 To 1
                RobotWalk(Robot, GravDir, RobotFrame) = "Sprites/Bot_" & Robot & "_" & GravDir & "_" & RobotFrame & ".gif"
            Next Robot
        Next RobotFrame
    Next GravDir
    
    'Store the file names of the level tile images.
    For Tile = 0 To 15
        TileTable(Tile) = "Tiles/Tile_" & Tile & ".gif"
    Next Tile
    ShockTile = "Tiles/Tile_Shock.gif"
    StartTile = "Tiles/Tile_Start.gif"
    FinishTile = "Tiles/Tile_Finish.gif"
    
    'Set the level number to zero and open the map file with its name corresponding to that number.
    Level = 0
  
    'Load the title screen.
    Call LoadLevel("Title")
    
    'Show the start button and make sure the timer is disabled.
    cmdStart.Visible = True
    cmdHelp.Visible = True
    cmdTopScores.Visible = True
    txtPlayerName.Visible = True
    tmrTimer.Enabled = False
    
    'Set the timer's interval to limit the game to 20 frames per second.
    tmrTimer.Interval = 1000 / 20
   
End Sub

Private Sub tmrTimer_Timer()

    'If the player's state is normal then...
    If PlayerState = 0 Then
        
        'Move the player first.
        Call MovePlayer
        
        'Calculates the player's falling speed.
        TickCount = TickCount + 1
        PlayerFallSpeed = IncreaseVelocity(0, 10, TickCount, 0.4)
        
        'Limit the player's maximum fall speed.
        If PlayerFallSpeed > 30 Then
            PlayerFallSpeed = 30
            TickCount = TickCount - 1
        End If
        
        'Drop the player (assuming there is no solid ground at their feet) and move the robots.
        Call DropPlayer
        Call MoveBot
        
        'Check to see if any of the robots are in contact with the player.
        Call CheckRobotContact
        
        'Reposition the player's form object according to the player's new position.
        imgPlayer.Left = PlayerPos(0) - imgPlayer.Width / 2
        imgPlayer.Top = PlayerPos(1) - imgPlayer.Height / 2
        
        'Repostition the robots' form objects according to their new positions.
        For a = 0 To 1
            imgRobot(a).Left = RobotPos(a, 0) - imgRobot(a).Width / 2
            imgRobot(a).Top = RobotPos(a, 1) - imgRobot(a).Height / 2
        Next a
        
        Call IncreaseClock
        
    'If the player has reached the exit...
    ElseIf PlayerState = 1 Then
        
        'Increase the success/fail report timer and set the level name label to report the player's success.
        ReportTimer = ReportTimer + 1
        lblLevelName.Caption = "Level Complete!"
        
        'After a few seconds increase the level number.
        If ReportTimer >= 90 Then
            Level = Level + 1
            'If the level number is that of the victory screen (itself a level map), return to the title screen.
            If Level >= 100 Then
                Call Form_Load
            'If the level number exceeds the last level, then load the victory screen.
            ElseIf Level >= 4 Then
                Level = 101
                Call LoadLevel("Win")
                PlayerStates.WinLevel
                Call WriteScore
            'Otherwise just load the next level.
            Else
                Call LoadLevel("Lab")
            End If
        End If
        
    'If the player failed...
    ElseIf PlayerState >= 2 Then
    
        'Increase the success/fail report timer.
        ReportTimer = ReportTimer + 1
        
        'Animate the player's current state.
        PlayerAnim = PlayerAnim + 1
        If PlayerAnim > 1 Then
            PlayerAnim = 0
        End If
        
        'Set the level name caption and player animation according to whether they have been dazed (from falls) or shocked.
        If PlayerState = 2 Then
            lblLevelName.Caption = "Oof!"
            imgPlayer.Picture = LoadPicture(PlayerDazed(PlayerGravityDir, PlayerAnim))
        Else
            lblLevelName.Caption = "Don't tase me, Robro!"
            imgPlayer.Picture = LoadPicture(PlayerShock(PlayerGravityDir, PlayerAnim))
        End If
        
        'After a few seconds, if the player still has lives, deduct one and reload the level.
        'Otherwise, return to the title screen. The game is over.
        If ReportTimer >= 90 Then
            If PlayerRetries > 0 Then
                PlayerRetries = PlayerRetries - 1
                lblRetries.Caption = "Retries: " & PlayerRetries
                Call LoadLevel("Lab")
            Else
                Call Form_Load
            End If
        End If
    End If
    
End Sub

'Sets the index of KeyIsDown corresponding to a key to True if that key is being pressed.
Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    KeyIsDown(KeyCode) = True
End Sub

'Sets the index of KeyIsDown corresponding to a key to False if that key has been released.
Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
    KeyIsDown(KeyCode) = False
End Sub

