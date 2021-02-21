VERSION 5.00
Object = "{40B5CE80-C5A8-11D2-8183-00002440DFD8}#7.12#0"; "KHAKES~1.OCX"
Begin VB.Form Form7 
   Appearance      =   0  'Flat
   AutoRedraw      =   -1  'True
   BackColor       =   &H00FFFFFF&
   BorderStyle     =   0  'None
   Caption         =   "High Score"
   ClientHeight    =   6180
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   8580
   Icon            =   "Form7.frx":0000
   LinkTopic       =   "Form7"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   Moveable        =   0   'False
   Picture         =   "Form7.frx":9E4A
   ScaleHeight     =   412
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   572
   StartUpPosition =   2  'CenterScreen
   Begin BTNENHLib4.BtnEnh BtnEnh3 
      Height          =   735
      Left            =   0
      TabIndex        =   0
      Top             =   5400
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
      textCaption     =   "Form7.frx":234C6
      textLT          =   "Form7.frx":2353E
      textCT          =   "Form7.frx":23556
      textRT          =   "Form7.frx":2356E
      textLM          =   "Form7.frx":23586
      textRM          =   "Form7.frx":2359E
      textLB          =   "Form7.frx":235B6
      textCB          =   "Form7.frx":235CE
      textRB          =   "Form7.frx":235E6
      colorBack       =   "Form7.frx":235FE
      colorIntern     =   "Form7.frx":23624
      colorMO         =   "Form7.frx":2364A
      colorFocus      =   "Form7.frx":23670
      colorDisabled   =   "Form7.frx":23696
      colorPressed    =   "Form7.frx":236BC
   End
   Begin BTNENHLib4.BtnEnh BtnEnh1 
      Height          =   735
      Left            =   3720
      TabIndex        =   1
      Top             =   1440
      Width           =   3135
      _Version        =   458764
      _ExtentX        =   5530
      _ExtentY        =   1296
      _StockProps     =   66
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
      Surface         =   9
      ShadowColor     =   65535
      SmoothEdges     =   2
      SpecialEffect   =   3
      LogPixels       =   96
      Clickable       =   0   'False
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form7.frx":236E2
      textLT          =   "Form7.frx":236FA
      textCT          =   "Form7.frx":23712
      textRT          =   "Form7.frx":2372A
      textLM          =   "Form7.frx":23742
      textRM          =   "Form7.frx":2375A
      textLB          =   "Form7.frx":23772
      textCB          =   "Form7.frx":2378A
      textRB          =   "Form7.frx":237A2
      colorBack       =   "Form7.frx":237BA
      colorIntern     =   "Form7.frx":237E0
      colorMO         =   "Form7.frx":23806
      colorFocus      =   "Form7.frx":2382C
      colorDisabled   =   "Form7.frx":23852
      colorPressed    =   "Form7.frx":23878
      Style           =   7
      Orientation     =   7
   End
   Begin BTNENHLib4.BtnEnh BtnEnh2 
      Height          =   735
      Left            =   4470
      TabIndex        =   2
      Top             =   2520
      Width           =   1575
      _Version        =   458764
      _ExtentX        =   2778
      _ExtentY        =   1296
      _StockProps     =   66
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
      Surface         =   9
      ShadowColor     =   65535
      SmoothEdges     =   2
      SpecialEffect   =   3
      LogPixels       =   96
      Clickable       =   0   'False
      UserData        =   41486.260150463
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form7.frx":2389E
      textLT          =   "Form7.frx":238B6
      textCT          =   "Form7.frx":238CE
      textRT          =   "Form7.frx":238E6
      textLM          =   "Form7.frx":238FE
      textRM          =   "Form7.frx":23916
      textLB          =   "Form7.frx":2392E
      textCB          =   "Form7.frx":23946
      textRB          =   "Form7.frx":2395E
      colorBack       =   "Form7.frx":23976
      colorIntern     =   "Form7.frx":2399C
      colorMO         =   "Form7.frx":239C2
      colorFocus      =   "Form7.frx":239E8
      colorDisabled   =   "Form7.frx":23A0E
      colorPressed    =   "Form7.frx":23A34
      Style           =   7
      Orientation     =   7
   End
   Begin VB.Image Image1 
      Height          =   2700
      Left            =   120
      Picture         =   "Form7.frx":23A5A
      Top             =   720
      Width           =   2700
   End
End
Attribute VB_Name = "Form7"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub BtnEnh3_Click()
Form7.Hide
Form1.Show
End Sub
