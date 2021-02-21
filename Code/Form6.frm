VERSION 5.00
Object = "{40B5CE80-C5A8-11D2-8183-00002440DFD8}#7.12#0"; "KHAKES~1.OCX"
Begin VB.Form Form6 
   Appearance      =   0  'Flat
   AutoRedraw      =   -1  'True
   BackColor       =   &H00FFFFFF&
   BorderStyle     =   0  'None
   Caption         =   "Options"
   ClientHeight    =   5550
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   8340
   Icon            =   "Form6.frx":0000
   LinkTopic       =   "Form6"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   Moveable        =   0   'False
   Picture         =   "Form6.frx":9E4A
   ScaleHeight     =   370
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   556
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox Text1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   24
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   735
      Left            =   3360
      TabIndex        =   10
      Text            =   "Player1"
      Top             =   3000
      Width           =   3135
   End
   Begin BTNENHLib4.BtnEnh BtnEnh2 
      Height          =   735
      Left            =   3360
      TabIndex        =   0
      Top             =   600
      Width           =   1500
      _Version        =   458764
      _ExtentX        =   2646
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Easy"
      BeginProperty FontCaption {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Shape           =   1
      Slant           =   0
      Surface         =   9
      PictureTranspMode=   0
      ShadowColor     =   33023
      SmoothEdges     =   2
      SpecialEffect   =   1
      LogPixels       =   96
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form6.frx":1541F
      textLT          =   "Form6.frx":15487
      textCT          =   "Form6.frx":1549F
      textRT          =   "Form6.frx":154B7
      textLM          =   "Form6.frx":154CF
      textRM          =   "Form6.frx":154E7
      textLB          =   "Form6.frx":154FF
      textCB          =   "Form6.frx":15517
      textRB          =   "Form6.frx":1552F
      colorBack       =   "Form6.frx":15547
      colorIntern     =   "Form6.frx":1556D
      colorMO         =   "Form6.frx":15593
      colorFocus      =   "Form6.frx":155B9
      colorDisabled   =   "Form6.frx":155DF
      colorPressed    =   "Form6.frx":15605
      Style           =   1
   End
   Begin BTNENHLib4.BtnEnh BtnEnh3 
      Height          =   735
      Left            =   4920
      TabIndex        =   1
      Top             =   600
      Width           =   1500
      _Version        =   458764
      _ExtentX        =   2646
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Normal"
      BeginProperty FontCaption {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Shape           =   1
      Slant           =   0
      Surface         =   9
      ShadowColor     =   65535
      SmoothEdges     =   2
      SpecialEffect   =   1
      LogPixels       =   96
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form6.frx":1562B
      textLT          =   "Form6.frx":15697
      textCT          =   "Form6.frx":156AF
      textRT          =   "Form6.frx":156C7
      textLM          =   "Form6.frx":156DF
      textRM          =   "Form6.frx":156F7
      textLB          =   "Form6.frx":1570F
      textCB          =   "Form6.frx":15727
      textRB          =   "Form6.frx":1573F
      colorBack       =   "Form6.frx":15757
      colorIntern     =   "Form6.frx":1577D
      colorMO         =   "Form6.frx":157A3
      colorFocus      =   "Form6.frx":157C9
      colorDisabled   =   "Form6.frx":157EF
      colorPressed    =   "Form6.frx":15815
      Style           =   1
   End
   Begin BTNENHLib4.BtnEnh BtnEnh4 
      Height          =   735
      Left            =   6480
      TabIndex        =   2
      Top             =   600
      Width           =   1500
      _Version        =   458764
      _ExtentX        =   2646
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Hard"
      BeginProperty FontCaption {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Shape           =   1
      Slant           =   0
      Surface         =   9
      ShadowColor     =   65535
      SmoothEdges     =   2
      SpecialEffect   =   1
      LogPixels       =   96
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form6.frx":1583B
      textLT          =   "Form6.frx":158A3
      textCT          =   "Form6.frx":158BB
      textRT          =   "Form6.frx":158D3
      textLM          =   "Form6.frx":158EB
      textRM          =   "Form6.frx":15903
      textLB          =   "Form6.frx":1591B
      textCB          =   "Form6.frx":15933
      textRB          =   "Form6.frx":1594B
      colorBack       =   "Form6.frx":15963
      colorIntern     =   "Form6.frx":15989
      colorMO         =   "Form6.frx":159AF
      colorFocus      =   "Form6.frx":159D5
      colorDisabled   =   "Form6.frx":159FB
      colorPressed    =   "Form6.frx":15A21
      Style           =   1
   End
   Begin BTNENHLib4.BtnEnh BtnEnh6 
      Height          =   735
      Left            =   3360
      TabIndex        =   3
      Top             =   1800
      Width           =   1500
      _Version        =   458764
      _ExtentX        =   2646
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "On"
      BeginProperty FontCaption {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Shape           =   1
      Slant           =   0
      Surface         =   9
      PictureTranspMode=   0
      ShadowColor     =   33023
      SmoothEdges     =   2
      SpecialEffect   =   1
      LogPixels       =   96
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form6.frx":15A47
      textLT          =   "Form6.frx":15AAB
      textCT          =   "Form6.frx":15AC3
      textRT          =   "Form6.frx":15ADB
      textLM          =   "Form6.frx":15AF3
      textRM          =   "Form6.frx":15B0B
      textLB          =   "Form6.frx":15B23
      textCB          =   "Form6.frx":15B3B
      textRB          =   "Form6.frx":15B53
      colorBack       =   "Form6.frx":15B6B
      colorIntern     =   "Form6.frx":15B91
      colorMO         =   "Form6.frx":15BB7
      colorFocus      =   "Form6.frx":15BDD
      colorDisabled   =   "Form6.frx":15C03
      colorPressed    =   "Form6.frx":15C29
      Style           =   1
   End
   Begin BTNENHLib4.BtnEnh BtnEnh7 
      Height          =   735
      Left            =   4920
      TabIndex        =   4
      Top             =   1800
      Width           =   1500
      _Version        =   458764
      _ExtentX        =   2646
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Off"
      BeginProperty FontCaption {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Shape           =   1
      Slant           =   0
      Surface         =   9
      PictureTranspMode=   0
      ShadowColor     =   33023
      SmoothEdges     =   2
      SpecialEffect   =   1
      LogPixels       =   96
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form6.frx":15C4F
      textLT          =   "Form6.frx":15CB5
      textCT          =   "Form6.frx":15CCD
      textRT          =   "Form6.frx":15CE5
      textLM          =   "Form6.frx":15CFD
      textRM          =   "Form6.frx":15D15
      textLB          =   "Form6.frx":15D2D
      textCB          =   "Form6.frx":15D45
      textRB          =   "Form6.frx":15D5D
      colorBack       =   "Form6.frx":15D75
      colorIntern     =   "Form6.frx":15D9B
      colorMO         =   "Form6.frx":15DC1
      colorFocus      =   "Form6.frx":15DE7
      colorDisabled   =   "Form6.frx":15E0D
      colorPressed    =   "Form6.frx":15E33
      Style           =   1
   End
   Begin BTNENHLib4.BtnEnh BtnEnh9 
      Height          =   735
      Left            =   0
      TabIndex        =   5
      Top             =   4800
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Back to menu"
      BeginProperty FontCaption {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Shape           =   1
      Surface         =   9
      ShadowColor     =   65535
      SmoothEdges     =   2
      SpecialEffect   =   3
      LogPixels       =   96
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form6.frx":15E59
      textLT          =   "Form6.frx":15ED1
      textCT          =   "Form6.frx":15EE9
      textRT          =   "Form6.frx":15F01
      textLM          =   "Form6.frx":15F19
      textRM          =   "Form6.frx":15F31
      textLB          =   "Form6.frx":15F49
      textCB          =   "Form6.frx":15F61
      textRB          =   "Form6.frx":15F79
      colorBack       =   "Form6.frx":15F91
      colorIntern     =   "Form6.frx":15FB7
      colorMO         =   "Form6.frx":15FDD
      colorFocus      =   "Form6.frx":16003
      colorDisabled   =   "Form6.frx":16029
      colorPressed    =   "Form6.frx":1604F
   End
   Begin BTNENHLib4.BtnEnh BtnEnh1 
      Height          =   735
      Left            =   120
      TabIndex        =   7
      Top             =   600
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Difficulti"
      BeginProperty FontCaption {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Shape           =   1
      Surface         =   2
      ShadowColor     =   65535
      SmoothEdges     =   2
      SpecialEffect   =   3
      LogPixels       =   96
      Clickable       =   0   'False
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form6.frx":16075
      textLT          =   "Form6.frx":160E9
      textCT          =   "Form6.frx":16101
      textRT          =   "Form6.frx":16119
      textLM          =   "Form6.frx":16131
      textRM          =   "Form6.frx":16149
      textLB          =   "Form6.frx":16161
      textCB          =   "Form6.frx":16179
      textRB          =   "Form6.frx":16191
      colorBack       =   "Form6.frx":161A9
      colorIntern     =   "Form6.frx":161CF
      colorMO         =   "Form6.frx":161F5
      colorFocus      =   "Form6.frx":1621B
      colorDisabled   =   "Form6.frx":16241
      colorPressed    =   "Form6.frx":16267
      Style           =   7
   End
   Begin BTNENHLib4.BtnEnh BtnEnh5 
      Height          =   735
      Left            =   120
      TabIndex        =   8
      Top             =   1800
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Sound"
      BeginProperty FontCaption {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Shape           =   1
      Surface         =   2
      ShadowColor     =   65535
      SmoothEdges     =   2
      SpecialEffect   =   3
      LogPixels       =   96
      Clickable       =   0   'False
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form6.frx":1628D
      textLT          =   "Form6.frx":162F7
      textCT          =   "Form6.frx":1630F
      textRT          =   "Form6.frx":16327
      textLM          =   "Form6.frx":1633F
      textRM          =   "Form6.frx":16357
      textLB          =   "Form6.frx":1636F
      textCB          =   "Form6.frx":16387
      textRB          =   "Form6.frx":1639F
      colorBack       =   "Form6.frx":163B7
      colorIntern     =   "Form6.frx":163DD
      colorMO         =   "Form6.frx":16403
      colorFocus      =   "Form6.frx":16429
      colorDisabled   =   "Form6.frx":1644F
      colorPressed    =   "Form6.frx":16475
      Style           =   7
   End
   Begin BTNENHLib4.BtnEnh BtnEnh8 
      Height          =   735
      Left            =   120
      TabIndex        =   9
      Top             =   3000
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Name"
      BeginProperty FontCaption {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRT {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRM {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextLB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextCB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty FontTextRB {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Shape           =   1
      Surface         =   2
      ShadowColor     =   65535
      SmoothEdges     =   2
      SpecialEffect   =   3
      LogPixels       =   96
      Clickable       =   0   'False
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form6.frx":1649B
      textLT          =   "Form6.frx":16503
      textCT          =   "Form6.frx":1651B
      textRT          =   "Form6.frx":16533
      textLM          =   "Form6.frx":1654B
      textRM          =   "Form6.frx":16563
      textLB          =   "Form6.frx":1657B
      textCB          =   "Form6.frx":16593
      textRB          =   "Form6.frx":165AB
      colorBack       =   "Form6.frx":165C3
      colorIntern     =   "Form6.frx":165E9
      colorMO         =   "Form6.frx":1660F
      colorFocus      =   "Form6.frx":16635
      colorDisabled   =   "Form6.frx":1665B
      colorPressed    =   "Form6.frx":16681
      Style           =   7
   End
   Begin VB.Label Label1 
      Caption         =   "1"
      Height          =   495
      Left            =   1800
      TabIndex        =   6
      Top             =   2040
      Visible         =   0   'False
      Width           =   1215
   End
End
Attribute VB_Name = "Form6"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False


Private Sub BtnEnh2_Click()
BtnEnh3.Value = 0
BtnEnh4.Value = 0
If BtnEnh2.Value = 0 And BtnEnh3.Value = 0 And BtnEnh4.Value = 0 Then
BtnEnh2.Value = 1
d = 1
Else
d = 1
End If
Label1.Caption = d
End Sub

Private Sub BtnEnh3_Click()
BtnEnh2.Value = 0
BtnEnh4.Value = 0
If BtnEnh2.Value = 0 And BtnEnh3.Value = 0 And BtnEnh4.Value = 0 Then
BtnEnh2.Value = 1
d = 1
Else
d = 2
End If
Label1.Caption = d
End Sub
Private Sub BtnEnh4_Click()
BtnEnh2.Value = 0
BtnEnh3.Value = 0
If BtnEnh2.Value = 0 And BtnEnh3.Value = 0 And BtnEnh4.Value = 0 Then
BtnEnh2.Value = 1
d = 1
Else
d = 3
End If
Label1.Caption = d
End Sub

Private Sub BtnEnh9_Click()
Form6.Hide
Form1.Show
End Sub
