VERSION 5.00
Object = "{40B5CE80-C5A8-11D2-8183-00002440DFD8}#7.12#0"; "KHAKES~1.OCX"
Object = "{82351433-9094-11D1-A24B-00A0C932C7DF}#1.5#0"; "GIF.ocx"
Begin VB.Form Form1 
   Appearance      =   0  'Flat
   AutoRedraw      =   -1  'True
   BackColor       =   &H80000005&
   BorderStyle     =   0  'None
   Caption         =   "Menu"
   ClientHeight    =   8100
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   10920
   Icon            =   "Form3.frx":0000
   LinkTopic       =   "Form3"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   Moveable        =   0   'False
   Picture         =   "Form3.frx":9E4A
   ScaleHeight     =   637.738
   ScaleMode       =   0  'User
   ScaleWidth      =   728
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   0
      Top             =   0
   End
   Begin BTNENHLib4.BtnEnh BtnEnh1 
      Height          =   735
      Left            =   3900
      TabIndex        =   0
      Top             =   2760
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Play"
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
      textCaption     =   "Form3.frx":234C6
      textLT          =   "Form3.frx":2352E
      textCT          =   "Form3.frx":23546
      textRT          =   "Form3.frx":2355E
      textLM          =   "Form3.frx":23576
      textRM          =   "Form3.frx":2358E
      textLB          =   "Form3.frx":235A6
      textCB          =   "Form3.frx":235BE
      textRB          =   "Form3.frx":235D6
      colorBack       =   "Form3.frx":235EE
      colorIntern     =   "Form3.frx":23614
      colorMO         =   "Form3.frx":2363A
      colorFocus      =   "Form3.frx":23660
      colorDisabled   =   "Form3.frx":23686
      colorPressed    =   "Form3.frx":236AC
   End
   Begin BTNENHLib4.BtnEnh BtnEnh2 
      Height          =   735
      Left            =   3900
      TabIndex        =   1
      Top             =   3720
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "High score"
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
      textCaption     =   "Form3.frx":236D2
      textLT          =   "Form3.frx":23746
      textCT          =   "Form3.frx":2375E
      textRT          =   "Form3.frx":23776
      textLM          =   "Form3.frx":2378E
      textRM          =   "Form3.frx":237A6
      textLB          =   "Form3.frx":237BE
      textCB          =   "Form3.frx":237D6
      textRB          =   "Form3.frx":237EE
      colorBack       =   "Form3.frx":23806
      colorIntern     =   "Form3.frx":2382C
      colorMO         =   "Form3.frx":23852
      colorFocus      =   "Form3.frx":23878
      colorDisabled   =   "Form3.frx":2389E
      colorPressed    =   "Form3.frx":238C4
   End
   Begin BTNENHLib4.BtnEnh BtnEnh3 
      Height          =   735
      Left            =   3900
      TabIndex        =   2
      Top             =   4680
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Gallery"
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
      textCaption     =   "Form3.frx":238EA
      textLT          =   "Form3.frx":23958
      textCT          =   "Form3.frx":23970
      textRT          =   "Form3.frx":23988
      textLM          =   "Form3.frx":239A0
      textRM          =   "Form3.frx":239B8
      textLB          =   "Form3.frx":239D0
      textCB          =   "Form3.frx":239E8
      textRB          =   "Form3.frx":23A00
      colorBack       =   "Form3.frx":23A18
      colorIntern     =   "Form3.frx":23A3E
      colorMO         =   "Form3.frx":23A64
      colorFocus      =   "Form3.frx":23A8A
      colorDisabled   =   "Form3.frx":23AB0
      colorPressed    =   "Form3.frx":23AD6
   End
   Begin BTNENHLib4.BtnEnh BtnEnh4 
      Height          =   735
      Left            =   3900
      TabIndex        =   3
      Top             =   5640
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Options"
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
      textCaption     =   "Form3.frx":23AFC
      textLT          =   "Form3.frx":23B6A
      textCT          =   "Form3.frx":23B82
      textRT          =   "Form3.frx":23B9A
      textLM          =   "Form3.frx":23BB2
      textRM          =   "Form3.frx":23BCA
      textLB          =   "Form3.frx":23BE2
      textCB          =   "Form3.frx":23BFA
      textRB          =   "Form3.frx":23C12
      colorBack       =   "Form3.frx":23C2A
      colorIntern     =   "Form3.frx":23C50
      colorMO         =   "Form3.frx":23C76
      colorFocus      =   "Form3.frx":23C9C
      colorDisabled   =   "Form3.frx":23CC2
      colorPressed    =   "Form3.frx":23CE8
   End
   Begin BTNENHLib4.BtnEnh BtnEnh5 
      Height          =   735
      Left            =   3900
      TabIndex        =   4
      Top             =   6600
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Exit"
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
      textCaption     =   "Form3.frx":23D0E
      textLT          =   "Form3.frx":23D76
      textCT          =   "Form3.frx":23D8E
      textRT          =   "Form3.frx":23DA6
      textLM          =   "Form3.frx":23DBE
      textRM          =   "Form3.frx":23DD6
      textLB          =   "Form3.frx":23DEE
      textCB          =   "Form3.frx":23E06
      textRB          =   "Form3.frx":23E1E
      colorBack       =   "Form3.frx":23E36
      colorIntern     =   "Form3.frx":23E5C
      colorMO         =   "Form3.frx":23E82
      colorFocus      =   "Form3.frx":23EA8
      colorDisabled   =   "Form3.frx":23ECE
      colorPressed    =   "Form3.frx":23EF4
   End
   Begin AniGIFCtrl.AniGIF AniGIF1 
      Height          =   195
      Left            =   120
      TabIndex        =   5
      Top             =   7800
      Visible         =   0   'False
      Width           =   10695
      BackColor       =   12632256
      PLaying         =   -1  'True
      Transparent     =   -1  'True
      Speed           =   1
      Stretch         =   0
      AutoSize        =   0   'False
      SequenceString  =   "Salam"
      Sequence        =   0
      HTTPProxy       =   ""
      HTTPUserName    =   ""
      HTTPPassword    =   ""
      MousePointer    =   0
      GIF             =   "Form3.frx":23F1A
      ExtendWidth     =   18865
      ExtendHeight    =   344
      Loop            =   0
      AutoRewind      =   -1  'True
      Synchronized    =   0   'False
   End
   Begin VB.Image Image2 
      Height          =   2445
      Left            =   2070
      Picture         =   "Form3.frx":2651AF
      Top             =   240
      Width           =   6795
   End
   Begin VB.Image Image1 
      Height          =   3375
      Left            =   240
      Picture         =   "Form3.frx":26A145
      Top             =   2280
      Width           =   3375
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim check
Public nam, best

Private Sub BtnEnh1_Click()
Form2.vy = 0
Form2.yy = 0
Form2.AniGIF1.Top = 0
score = 0

Form2.Label2.Caption = 0
Form2.Label1.Caption = 0

Form2.Line1(1).x1 = 152: Form2.Line1(1).x2 = 152
Form2.Line1(1).y1 = 280: Form2.Line1(1).y2 = 192
Form2.Line1(1).Visible = True

Form2.Line1(2).x1 = 1144: Form2.Line1(2).x2 = 1144
Form2.Line1(2).y1 = 280: Form2.Line1(2).y2 = 192
Form2.Line1(2).Visible = True

Form2.Line1(3).x1 = 792: Form2.Line1(3).x2 = 792
Form2.Line1(3).y1 = 0: Form2.Line1(3).y2 = 95
Form2.Line1(3).Visible = True

Form2.Line1(4).x1 = 432: Form2.Line1(4).x2 = 432
Form2.Line1(4).y1 = 0: Form2.Line1(4).y2 = 52
Form2.Line1(4).Visible = True

AniGIF1.Visible = True
AniGIF1.Playing = True
Timer1.Enabled = True
End Sub

Private Sub BtnEnh2_Click()
Form1.Hide
Form7.Show
file = "D:\Line Bird\Save\save.txt"
Open file For Input As #1
Input #1, nam
Input #1, best
Close #1
Form7.BtnEnh1.Caption = nam
Form7.BtnEnh2.Caption = best
End Sub

Private Sub BtnEnh3_Click()
Form1.Hide
Form5.Show
End Sub

Private Sub BtnEnh4_Click()
Form1.Hide
Form6.Show
End Sub

Private Sub BtnEnh5_Click()
End
End Sub

Private Sub Timer1_Timer()
check = check + 1
If check = 50 Then
Timer1.Enabled = False
Form1.Hide
Form2.Show
AniGIF1.Visible = False
check = 0
End If
End Sub
