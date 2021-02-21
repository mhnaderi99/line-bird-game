VERSION 5.00
Object = "{40B5CE80-C5A8-11D2-8183-00002440DFD8}#7.12#0"; "KHAKES~1.OCX"
Object = "{82351433-9094-11D1-A24B-00A0C932C7DF}#1.5#0"; "GIF.ocx"
Begin VB.Form Form2 
   Appearance      =   0  'Flat
   AutoRedraw      =   -1  'True
   BackColor       =   &H00400000&
   BorderStyle     =   0  'None
   Caption         =   "Line Bird"
   ClientHeight    =   9780
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   17700
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   15.75
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00000000&
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   652
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   1180
   StartUpPosition =   2  'CenterScreen
   Begin BTNENHLib4.BtnEnh BtnEnh1 
      Height          =   750
      Left            =   16920
      TabIndex        =   3
      Top             =   8970
      Width           =   750
      _Version        =   458764
      _ExtentX        =   1323
      _ExtentY        =   1323
      _StockProps     =   66
      Caption         =   "l l"
      BeginProperty FontCaption {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial Narrow"
         Size            =   24
         Charset         =   0
         Weight          =   700
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
      FocusMode       =   1
      LogPixels       =   96
      ForeColorDisabled=   16761024
      UserData        =   41486.1440046296
      Res             =   1360
      BitsPix         =   32
      textCaption     =   "Form1.frx":9E4A
      textLT          =   "Form1.frx":9EB0
      textCT          =   "Form1.frx":9EC8
      textRT          =   "Form1.frx":9EE0
      textLM          =   "Form1.frx":9EF8
      textRM          =   "Form1.frx":9F10
      textLB          =   "Form1.frx":9F28
      textCB          =   "Form1.frx":9F40
      textRB          =   "Form1.frx":9F58
      colorBack       =   "Form1.frx":9F70
      colorIntern     =   "Form1.frx":9F96
      colorMO         =   "Form1.frx":9FBC
      colorFocus      =   "Form1.frx":9FE2
      colorDisabled   =   "Form1.frx":A008
      colorPressed    =   "Form1.frx":A02E
   End
   Begin VB.Timer Timer2 
      Enabled         =   0   'False
      Interval        =   1000
      Left            =   27000
      Top             =   4800
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   9780
      Left            =   0
      Picture         =   "Form1.frx":A054
      ScaleHeight     =   267.019
      ScaleMode       =   0  'User
      ScaleWidth      =   1179
      TabIndex        =   0
      Top             =   0
      Width           =   17715
      Begin AniGIFCtrl.AniGIF AniGIF1 
         Height          =   855
         Left            =   720
         TabIndex        =   2
         Top             =   0
         Width           =   1215
         BackColor       =   12632256
         PLaying         =   -1  'True
         Transparent     =   -1  'True
         Speed           =   1
         Stretch         =   0
         AutoSize        =   0   'False
         SequenceString  =   ""
         Sequence        =   0
         HTTPProxy       =   ""
         HTTPUserName    =   ""
         HTTPPassword    =   ""
         MousePointer    =   0
         GIF             =   "Form1.frx":236D0
         ExtendWidth     =   2143
         ExtendHeight    =   1508
         Loop            =   0
         AutoRewind      =   0   'False
         Synchronized    =   -1  'True
      End
      Begin VB.Label Label2 
         BackColor       =   &H00000000&
         Caption         =   "0"
         BeginProperty Font 
            Name            =   "Lucida Handwriting"
            Size            =   15.75
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   495
         Left            =   16320
         TabIndex        =   6
         Top             =   120
         Width           =   1215
      End
      Begin VB.Line Line1 
         BorderWidth     =   15
         Index           =   3
         X1              =   1144
         X2              =   1144
         Y1              =   280
         Y2              =   192.007
      End
      Begin VB.Label Label3 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00000000&
         BorderStyle     =   1  'Fixed Single
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   72
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   2175
         Left            =   6600
         TabIndex        =   5
         Top             =   3840
         Visible         =   0   'False
         Width           =   5655
         WordWrap        =   -1  'True
      End
      Begin VB.Line Line1 
         BorderWidth     =   15
         Index           =   1
         X1              =   152
         X2              =   152
         Y1              =   280
         Y2              =   192.007
      End
      Begin VB.Line Line1 
         BorderWidth     =   15
         Index           =   4
         X1              =   432
         X2              =   432
         Y1              =   0
         Y2              =   52.007
      End
      Begin VB.Line Line1 
         BorderWidth     =   15
         Index           =   2
         X1              =   792
         X2              =   792
         Y1              =   0
         Y2              =   95.004
      End
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   14040
      Top             =   0
   End
   Begin VB.CommandButton Command1 
      Appearance      =   0  'Flat
      BackColor       =   &H000000FF&
      Caption         =   "Start"
      Height          =   360
      Left            =   0
      Style           =   1  'Graphical
      TabIndex        =   1
      Top             =   -960
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label Label1 
      BackColor       =   &H00400000&
      Caption         =   "0"
      Height          =   495
      Left            =   9240
      TabIndex        =   4
      Top             =   10440
      Visible         =   0   'False
      Width           =   1215
   End
End
Attribute VB_Name = "Form2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim xl1(4), xl2(4), yl1(4), yl2(4), vxl, vyl
Public score
Dim x1(4), x2(4), y1(4), y2(4)
Public h, xx, yy, vx, vy, ax, ay, m, s, flag, l, k, u, d

Private Sub BtnEnh1_Click()
Timer1.Enabled = False
AniGIF1.Playing = False
Form3.Show
End Sub

Private Sub Form_Load()
Randomize Timer
l = 1
d = 1
For i = 1 To 4
x1(i) = Line1(i).x1
x2(i) = Line1(i).x2
y1(i) = Line1(i).y1
y2(i) = Line1(i).y2
Next i
AniGIF1.Top = 0
s = 0
flag = 0
score = 0
h = 0.1
xx = AniGIF1.Left
yy = 0
vx = 0
vy = 0
ax = 0
ay = 20
vxl = 10
vyl = 0
For i = 1 To 4
xl1(i) = Line1(i).x1: xl2(i) = Line1(i).x2
yl1(i) = Line1(i).y1: yl2(i) = Line1(i).y2
Next i
Label2.Caption = score
End Sub

Private Sub Picture1_KeyDown(KeyCode As Integer, Shift As Integer)
If KeyCode = vbKeyUp Then
vy = -20
End If
If KeyCode = vbKeyRight Then
Label1.Caption = Label1.Caption + 1
If Label1.Caption = 1 Then
If Command1.Caption <> "Submit" Then
score = 0
vy = 0
vxl = 10
Timer1.Enabled = True
yy = 0
For i = 1 To 4
xl1(i) = Line1(i).x1: xl2(i) = Line1(i).x2
yl1(i) = Line1(i).y1: yl2(i) = Line1(i).y2
Next i
Else
Form1.Hide
Form2.Show
End If
End If
End If

End Sub

Private Sub Timer1_Timer()
s = s + 1
If s Mod 10 = 0 And vxl < 30 * Form6.Label1.Caption Then
vxl = vxl + Form6.Label1.Caption / 10
End If
For i = 1 To 4
  xl1(i) = xl1(i) - vxl * h: xl2(i) = xl2(i) - vxl * h
  yl1(i) = yl1(i) + vyl * h: yl2(i) = yl2(i) + vyl * h

  If xl1(i) <= 0 Then
    Line1(i).Visible = False
    j = Int(Rnd * 3)
    If j = 1 Or j = 0 Then
      yl1(i) = 0
      yl2(i) = Int(Rnd * (Picture1.ScaleHeight - AniGIF1.Height - 20))
    End If
    If j = 2 Then
      yl1(i) = Picture1.ScaleHeight
      yl2(i) = Int(Rnd * (Picture1.ScaleHeight - AniGIF1.Height)) + AniGIF1.Height + 20
    End If
    xl1(i) = Picture1.ScaleWidth
    xl2(i) = Picture1.ScaleWidth
    If j <> 0 Then Line1(i).Visible = True
  End If
Next i


vx = vx + ax * h
vy = vy + ay * h
xx = xx + vx * h
yy = yy + vy * h

If AniGIF1.Top + AniGIF1.Height >= Picture1.ScaleWidth Then Timer1.Enabled = False: For j = 1 To 10000000: Next j: Form4.Label1.Caption = score + 1: Form4.Show
If AniGIF1.Top + AniGIF1.Height <= 0 Then Timer1.Enabled = False: For j = 1 To 10000000: Next j: Form4.Label1.Caption = score + 1: Form4.Show

For i = 1 To 4

If yl1(i) = 0 And Line1(i).Visible = True Then
If AniGIF1.Left + AniGIF1.Width > xl1(i) - 3 And AniGIF1.Left + AniGIF1.Width < xl1(i) + 3 And AniGIF1.Top < yl2(i) Then
Timer1.Enabled = False: For j = 1 To 10000000: Next j: Form4.Label1.Caption = score + 1: Form4.Show
End If
End If

If yl1(i) = Picture1.ScaleHeight And Line1(i).Visible = True Then
If AniGIF1.Left + AniGIF1.Width > xl1(i) - 3 And AniGIF1.Left + AniGIF1.Width < xl1(i) + 3 And AniGIF1.Top + AniGIF1.Height > yl2(i) Then
Timer1.Enabled = False: For j = 1 To 10000000: Next j: Form4.Label1.Caption = score + 1: Form4.Show
End If
End If

Next i

AniGIF1.Left = xx
AniGIF1.Top = yy

For i = 1 To 4
Line1(i).x1 = xl1(i): Line1(i).x2 = xl2(i)
Line1(i).y1 = yl1(i): Line1(i).y2 = yl2(i)
Next i

score = score + 1
Label2.Caption = score

End Sub


Private Sub Timer2_Timer()
u = u + 1
If u < 4 Then Label3.Caption = 4 - u

If u = 4 Then
Timer1.Enabled = True
AniGIF1.Playing = True
Label3.Visible = False
Label3.Caption = "Ready"
u = 0
Timer2.Enabled = False
End If
End Sub
