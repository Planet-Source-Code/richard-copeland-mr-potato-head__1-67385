VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00FFFFFF&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Mr Potato Head"
   ClientHeight    =   7680
   ClientLeft      =   1155
   ClientTop       =   1860
   ClientWidth     =   8400
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   Picture         =   "Form1.frx":0000
   ScaleHeight     =   512
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   560
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Image Image1 
      Height          =   960
      Index           =   20
      Left            =   4560
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":2BCB
      Top             =   0
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   19
      Left            =   3240
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":844D
      Top             =   240
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   15
      Left            =   360
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":DCCF
      Top             =   3000
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   285
      Index           =   13
      Left            =   960
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":13551
      Top             =   4560
      Width           =   435
   End
   Begin VB.Image Image1 
      Height          =   315
      Index           =   14
      Left            =   240
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":14567
      Top             =   4560
      Width           =   495
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   7
      Left            =   6480
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":15901
      Top             =   6000
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   6
      Left            =   7320
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":1B183
      Top             =   6000
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   4
      Left            =   7320
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":20A05
      Top             =   4320
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   5
      Left            =   6360
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":26287
      Top             =   4320
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   3
      Left            =   6840
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":2BB09
      Top             =   3000
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   630
      Index           =   2
      Left            =   6840
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":3138B
      Top             =   2400
      Width           =   825
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   1
      Left            =   7320
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":347F5
      Top             =   1080
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   21
      Left            =   5760
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":3A077
      Top             =   120
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   17
      Left            =   720
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":3F8F9
      Top             =   360
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   18
      Left            =   1920
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":4517B
      Top             =   120
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   960
      Index           =   16
      Left            =   240
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":4A9FD
      Top             =   1560
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   585
      Index           =   11
      Left            =   360
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":5027F
      Top             =   5760
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   645
      Index           =   9
      Left            =   480
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":53A31
      Top             =   6720
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   555
      Index           =   12
      Left            =   360
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":57723
      Top             =   4920
      Width           =   960
   End
   Begin VB.Image Image1 
      Height          =   735
      Index           =   10
      Left            =   1680
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":5AC35
      Top             =   6600
      Width           =   495
   End
   Begin VB.Image Image1 
      Height          =   735
      Index           =   8
      Left            =   2280
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":5D3EF
      Top             =   6600
      Width           =   495
   End
   Begin VB.Image Image1 
      Appearance      =   0  'Flat
      Height          =   720
      Index           =   0
      Left            =   7320
      MousePointer    =   2  'Cross
      Picture         =   "Form1.frx":5FBA9
      Top             =   120
      Width           =   720
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private InitX As Integer
Private InitY As Integer

Private Sub Image1_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
InitX = X / Screen.TwipsPerPixelX
InitY = Y / Screen.TwipsPerPixelY
End Sub

Private Sub Image1_MouseMove(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
If Button = 1 Then
    Image1(Index).Left = Image1(Index).Left + ((X / Screen.TwipsPerPixelX) - InitX)
    Image1(Index).Top = Image1(Index).Top + ((Y / Screen.TwipsPerPixelY) - InitY)

End If
End Sub
