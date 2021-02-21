VERSION 5.00
Object = "{40B5CE80-C5A8-11D2-8183-00002440DFD8}#7.12#0"; "KHAKES~1.OCX"
Begin VB.Form Form3 
   BackColor       =   &H00FFFFFF&
   BorderStyle     =   0  'None
   Caption         =   "Pause Menu"
   ClientHeight    =   5520
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   5760
   Icon            =   "Form2.frx":0000
   LinkTopic       =   "Form3"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   Moveable        =   0   'False
   Picture         =   "Form2.frx":9E4A
   ScaleHeight     =   368
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   384
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin BTNENHLib4.BtnEnh BtnEnh1 
      Height          =   735
      Left            =   1320
      TabIndex        =   0
      Top             =   1200
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Resume"
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
      textCaption     =   "Form2.frx":234C6
      textLT          =   "Form2.frx":23532
      textCT          =   "Form2.frx":2354A
      textRT          =   "Form2.frx":23562
      textLM          =   "Form2.frx":2357A
      textRM          =   "Form2.frx":23592
      textLB          =   "Form2.frx":235AA
      textCB          =   "Form2.frx":235C2
      textRB          =   "Form2.frx":235DA
      colorBack       =   "Form2.frx":235F2
      colorIntern     =   "Form2.frx":23618
      colorMO         =   "Form2.frx":2363E
      colorFocus      =   "Form2.frx":23664
      colorDisabled   =   "Form2.frx":2368A
      colorPressed    =   "Form2.frx":236B0
   End
   Begin BTNENHLib4.BtnEnh BtnEnh2 
      Height          =   735
      Left            =   1320
      TabIndex        =   1
      Top             =   2280
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Restart"
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
      textCaption     =   "Form2.frx":236D6
      textLT          =   "Form2.frx":23744
      textCT          =   "Form2.frx":2375C
      textRT          =   "Form2.frx":23774
      textLM          =   "Form2.frx":2378C
      textRM          =   "Form2.frx":237A4
      textLB          =   "Form2.frx":237BC
      textCB          =   "Form2.frx":237D4
      textRB          =   "Form2.frx":237EC
      colorBack       =   "Form2.frx":23804
      colorIntern     =   "Form2.frx":2382A
      colorMO         =   "Form2.frx":23850
      colorFocus      =   "Form2.frx":23876
      colorDisabled   =   "Form2.frx":2389C
      colorPressed    =   "Form2.frx":238C2
   End
   Begin BTNENHLib4.BtnEnh BtnEnh3 
      Height          =   735
      Left            =   1320
      TabIndex        =   2
      Top             =   3360
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
      Caption         =   "Return to menu"
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
      textCaption     =   "Form2.frx":238E8
      textLT          =   "Form2.frx":23964
      textCT          =   "Form2.frx":2397C
      textRT          =   "Form2.frx":23994
      textLM          =   "Form2.frx":239AC
      textRM          =   "Form2.frx":239C4
      textLB          =   "Form2.frx":239DC
      textCB          =   "Form2.frx":239F4
      textRB          =   "Form2.frx":23A0C
      colorBack       =   "Form2.frx":23A24
      colorIntern     =   "Form2.frx":23A4A
      colorMO         =   "Form2.frx":23A70
      colorFocus      =   "Form2.frx":23A96
      colorDisabled   =   "Form2.frx":23ABC
      colorPressed    =   "Form2.frx":23AE2
   End
End
Attribute VB_Name = "Form3"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub BtnEnh1_Click()
u = 0
Form2.Label3.Caption = "Ready"
Form3.Hide
Form2.Label3.Visible = True
Form2.Timer2.Enabled = True
End Sub

Private Sub BtnEnh2_Click()
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

Form2.vy = 0
Form2.yy = 0
Form2.AniGIF1.Top = 0
Form3.Hide
Form2.Label1.Caption = 0
Form2.Label2.Caption = 0
Form2.AniGIF1.Playing = True
score = 0
End Sub

Private Sub BtnEnh3_Click()
Form3.Hide
Form2.Hide
Form1.Show
End Sub
