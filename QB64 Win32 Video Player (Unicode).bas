Option Explicit
$NoPrefix
$Console:Only
Console Off

$VersionInfo:CompanyName=SpriggsySpriggs
$VersionInfo:FileDescription=QB64 Win32 Video Player (Unicode)
$VersionInfo:ProductName=QB64 Win32 Video Player (Unicode)
$VersionInfo:ProductVersion=0,0,4,0
$VersionInfo:FileVersion=0,0,4,0
$VersionInfo:LegalCopyright=2022 SpriggsySpriggs
$VersionInfo:Web=https://github.com/SpriggsySpriggs/QB64-Win32-Video-Player
$VersionInfo:Comments=The Unicode build of the QB64 Win32 Video Player

'$Include:'Resources\Includes\OpenSaveW.BI'
'$Include:'Resources\Includes\TaskDlg.BI'
'$Include:'Resources\Includes\MCI.BI'

Const IDI_APPLICATION = 32512
Const IDC_ARROW = 32512
Const COLOR_WINDOW = 5

Const WS_OVERLAPPED = &H00000000
Const WS_CAPTION = &H00C00000
Const WS_SYSMENU = &H00080000
Const WS_THICKFRAME = &H00040000
Const WS_MINIMIZEBOX = &H00020000
Const WS_MAXIMIZEBOX = &H00010000
Const WS_OVERLAPPEDWINDOW = WS_OVERLAPPED Or WS_CAPTION Or WS_SYSMENU Or WS_THICKFRAME Or WS_MINIMIZEBOX Or WS_MAXIMIZEBOX
Const CW_USEDEFAULT = &H80000000
Const WS_TABSTOP = &H00010000
Const WS_CHILD = &H40000000
Const WS_VISIBLE = &H10000000

Const WC_BUTTON = "Button"
Const BS_DEFPUSHBUTTON = &H00000001
Const BS_BITMAP = &H00000080

Const WM_DESTROY = 2
Const WM_GETTEXT = &H000D
Const WM_CLOSE = &H0010
Const WM_COMMAND = &H0111
Const WM_CREATE = &H0001
Const WM_TIMER = &H0113
Const WM_DROPFILES = &H0233
Const WM_SETFONT = &H0030
Const WM_MENUSELECT = &H011F
Const WM_SIZE = &H0005
Const WM_HSCROLL = &H0114

Const IDCANCEL = 2
Const IDNO = 7
Const IDOK = 1
Const IDRETRY = 4
Const IDYES = 6

Const WM_USER = &H0400
Const TRACKBAR_CLASS = "msctls_trackbar32"
Const TBM_SETRANGE = WM_USER + 6
Const TBM_SETPOS = WM_USER + 5

Const WC_EDIT = "Edit"
Const WM_SETTEXT = &H000C

Const SW_SHOWNORMAL = 1
Const SW_SHOWDEFAULT = &HA

Const MF_STRING = &H00000000

Const AW_ACTIVATE = &H00020000
Const AW_BLEND = &H00080000
Const AW_CENTER = &H00000010
Const AW_HIDE = &H00010000
Const AW_HOR_POSITIVE = &H00000001
Const AW_HOR_NEGATIVE = &H00000002
Const AW_SLIDE = &H00040000
Const AW_VER_POSITIVE = &H00000004
Const AW_VER_NEGATIVE = &H00000008

Type POINT
    As Long x, y
End Type

Type MSG
    As Offset hwnd
    As Unsigned Long message
    As Unsigned Offset wParam
    As Offset lParam
    As Long time
    As POINT pt
    As Long lPrivate
End Type

Type WNDCLASSEX
    As Unsigned Long cbSize, style
    As Offset lpfnWndProc
    As Long cbClsExtra, cbWndExtra
    As Offset hInstance, hIcon, hCursor, hbrBackground, lpszMenuName, lpszClassName, hIconSm
End Type

Type RECT
    As Long left, top, right, bottom
End Type

Declare CustomType Library
    Function GetModuleHandle%& (ByVal lpModulename As Offset)
    Function RegisterClassEx% (ByVal wndclassex As Offset)
    Function CreateWindowEx%& (ByVal dwExStyle As Long, Byval lpClassName As Offset, Byval lpWindowName As Offset, Byval dwStyle As Long, Byval X As Long, Byval Y As Long, Byval nWidth As Long, Byval nHeight As Long, Byval hWndParent As Offset, Byval wMenu As Offset, Byval hInstance As Offset, Byval lpParam As Offset)
    Function LoadCursor%& (ByVal hInstance As Offset, Byval lpCursorName As Offset)
    Sub SendMessage (ByVal hWnd As Offset, Byval Msg As Unsigned Long, Byval wParam As Unsigned Offset, Byval lParam As Offset)
    Function SendMessage%& (ByVal hWnd As Offset, Byval Msg As Unsigned Long, Byval wParam As Unsigned Offset, Byval lParam As Offset)
    Function DefWindowProc%& (ByVal hWnd As Offset, Byval Msg As Unsigned Long, Byval wParam As Unsigned Offset, Byval lParam As Offset)
    Sub PostQuitMessage (ByVal nExitCode As Long)
    Sub ShowWindow (ByVal hWnd As Offset, Byval nCmdShow As Long)
    Sub UpdateWindow (ByVal hWnd As Offset)
    Function GetMessage%% (ByVal lpMsg As Offset, Byval hWnd As Offset, Byval wMsgFilterMin As Unsigned Long, Byval wMsgFilterMax As Unsigned Long)
    Sub TranslateMessage (ByVal lpMsg As Offset)
    Sub DispatchMessage (ByVal lpMsg As Offset)
    Sub EnableWindow (ByVal hWnd As Offset, Byval bEnable As Long)
    Sub GetWindowRect (ByVal hWnd As Offset, Byval lpRect As Offset)
    Sub SetWindowPos (ByVal hWnd As Offset, Byval hWndInsertAfter As Offset, Byval X As Long, Byval Y As Long, Byval cx As Long, Byval cy As Long, Byval uFlags As Unsigned Long)
    Sub GetWindowTextW (ByVal hWnd As Offset, Byval lpString As Offset, Byval nMaxCount As Long)
    Sub SetWindowTextW (ByVal hWnd As Offset, Byval lpString As Offset)
    Sub SetWindowTextA (ByVal hWnd As Offset, Byval lpString As Offset)
    Function GetWindowLongPtr%& (ByVal hWnd As Offset, Byval nIndex As Long)
    Sub SetWindowLongPtr (ByVal hWnd As Offset, Byval nIndex As Long, Byval dwNewLong As Offset)
    Sub RedrawWindow (ByVal hWnd As Offset, Byval lprcUpdate As Offset, Byval hrgnUpdate As Offset, Byval flags As Unsigned Long)
    Sub SetTimer (ByVal hWnd As Offset, Byval nIDEvent As Unsigned Offset, Byval uElapse As Unsigned Long, Byval lpTimerFunc As Offset)
    Sub ClientToScreen (ByVal hWnd As Offset, Byval lpPoint As Offset)
    Sub ShellExecuteW (ByVal hwnd As Offset, Byval lpOperation As Offset, Byval lpFile As Offset, Byval lpParameters As Offset, Byval lpDirectory As Offset, Byval nShowCmd As Long)
    Function CreateMenu%& ()
    Sub SetMenu (ByVal hWnd As Offset, Byval hMenu As Offset)
    Sub AppendMenu (ByVal hMenu As Offset, Byval uFlags As Long, Byval uIDNewItem As Unsigned Long, Byval lpNewItem As Offset)
    Sub AnimateWindow (ByVal hwnd As Offset, Byval Time As Unsigned Long, Byval Flags As Unsigned Long)
    Sub MoveWindow (ByVal hWnd As Offset, Byval X As Long, Byval Y As Long, Byval nWidth As Long, Byval nHeight As Long, Byval bRepaint As Long)
End Declare

Declare Library
    Function MAKELPARAM%& (ByVal l As Integer, Byval h As Integer)
    Function LOWORD~% (ByVal dwValue As Unsigned Long)
    Function __RGB~& Alias "RGB" (ByVal r As Unsigned Byte, Byval g As Unsigned Byte, Byval b As Unsigned Byte)
End Declare

Declare Dynamic Library "Gdi32"
    Function CreateFont%& Alias "CreateFontA" (ByVal cHeight As Long, Byval cWidth As Long, Byval cEscapement As Long, Byval cOrientation As Long, Byval cWeight As Long, Byval bItalic As Unsigned Long, Byval bUnderline As Unsigned Long, Byval bStrikeout As Unsigned Long, Byval iCharSet As Unsigned Long, Byval iOutPrecision As Unsigned Long, Byval iClipPrecision As Unsigned Long, Byval iQuality As Unsigned Long, Byval iPitchAndFamily As Unsigned Long, pszFaceName As String)
    Function CreateSolidBrush%& (ByVal color As Offset)
End Declare

Declare CustomType Library ".\internal\c\c_compiler\x86_64-w64-mingw32\include\shellapi"
    Function ExtractIcon%& (ByVal hInst As Offset, pszExeFileName As String, Byval nIconIndex As Unsigned Long)
End Declare

Declare Library "win"
    Function GetWindowProc%& ()
End Declare

Const DEFEXT = "mp4"

$ExeIcon:'.\Resources\Icons\video-vintage.ico'
Icon

CreateIcons
SetConsoleUnicode

Dim Shared As Offset parentWin, hInstance, videoWin, playpauseBtn, seekLbtn, seekRbtn, trackbar 'openBtn,
Dim Shared As Unsigned Long AboutMenu: AboutMenu = 99
Dim Shared As Long isOpen: isOpen = 0
hInstance = GetModuleHandle(0)

StartUp

Sub StartUp ()
    Dim As MSG msg

    Dim As WNDCLASSEX wc
    Dim As String className: className = "main" + Chr$(0)
    wc.cbSize = Len(wc)
    wc.lpfnWndProc = GetWindowProc
    wc.hInstance = hInstance
    wc.hCursor = LoadCursor(0, MAKEINTRESOURCE(IDC_ARROW))
    wc.hIcon = ExtractIcon(0, CWD$ + "\Resources\Icons\video-vintage.ico" + Chr$(0), 0)
    wc.hbrBackground = COLOR_WINDOW + 1
    wc.lpszClassName = Offset(className)
    wc.hbrBackground = CreateSolidBrush(__RGB(0, 0, 0))

    Dim As Integer reg: reg = RegisterClassEx(Offset(wc))
    If reg = 0 Then System

    Dim As String parentTitle: parentTitle = "QB64 Win32 Video Player" + Chr$(0)
    parentWin = CreateWindowEx(0, MAKELPARAM(reg, 0), Offset(parentTitle), &H00010000 Or WS_OVERLAPPED Or WS_CAPTION Or WS_SYSMENU Or WS_MINIMIZEBOX, CW_USEDEFAULT, CW_USEDEFAULT, 1280, 900, 0, 0, hInstance, 0)
    If parentWin = 0 Then System

    Dim As String class: class = WC_BUTTON + Chr$(0)
    playpauseBtn = CreateWindowEx(0, Offset(class), 0, WS_TABSTOP Or WS_VISIBLE Or WS_CHILD Or BS_BITMAP, (WindowWidth(parentWin) / 2) - 20, WindowHeight(parentWin) - 95, 40, 40, parentWin, 0, hInstance, 0)
    seekLbtn = CreateWindowEx(0, Offset(class), 0, WS_TABSTOP Or WS_VISIBLE Or WS_CHILD Or BS_BITMAP, (WindowWidth(parentWin) / 2) - 60, WindowHeight(parentWin) - 95, 40, 40, parentWin, 0, hInstance, 0)
    seekRbtn = CreateWindowEx(0, Offset(class), 0, WS_TABSTOP Or WS_VISIBLE Or WS_CHILD Or BS_BITMAP, (WindowWidth(parentWin) / 2) + 20, WindowHeight(parentWin) - 95, 40, 40, parentWin, 0, hInstance, 0)
    class = TRACKBAR_CLASS + Chr$(0)
    trackbar = CreateWindowEx(0, Offset(class), 0, WS_CHILD Or WS_VISIBLE, 5, WindowHeight(parentWin) - 115, WindowWidth(parentWin) - 20, 20, parentWin, 0, hInstance, 0)

    ChangeIcon playpauseBtn, "Resources\Icons\play-pause.ico"
    ChangeIcon seekRbtn, "Resources\Icons\fast-forward-30.ico"
    ChangeIcon seekLbtn, "Resources\Icons\rewind-10.ico"

    ToggleEnable playpauseBtn, 0
    ToggleEnable seekLbtn, 0
    ToggleEnable seekRbtn, 0
    ToggleEnable trackbar, 0

    SendMessage trackbar, TBM_SETRANGE, 0, MAKELPARAM(0, 100)

    Dim As Offset hMenu: hMenu = CreateMenu
    Dim As String openString: openString = "Open" + Chr$(0)
    Dim As String aboutMenuString: aboutMenuString = "About" + Chr$(0)
    AppendMenu hMenu, MF_STRING, AboutMenu + 1, Offset(openString)
    AppendMenu hMenu, MF_STRING, AboutMenu, Offset(aboutMenuString)
    SetMenu parentWin, hMenu

    CenterWindow parentWin
    FadeIn parentWin, 500
    UpdateWindow parentWin

    If CommandCount Then
        Dim As Offset arg: arg = CommandW
        If FileExistsW(arg) Then
            wprintf arg
            qPlay PointerToWideString(arg)
            videoWin = VideoHandle
            ToggleEnable playpauseBtn, 1
            ToggleEnable seekLbtn, 1
            ToggleEnable seekRbtn, 1
        End If
    End If

    SetTimer parentWin, 1, 500, 0

    While GetMessage(Offset(msg), 0, 0, 0)
        TranslateMessage Offset(msg)
        DispatchMessage Offset(msg)
    Wend

    System Val(Str$(msg.wParam))
End Sub

Sub CreateIcons ()
    __fastforward
    __playpause
    __rewind
    __movieopen
End Sub
'$Include:'Resources\Includes\fast-forward-30.ico.BM'
'$Include:'Resources\Includes\play-pause.ico.BM'
'$Include:'Resources\Includes\rewind-10.ico.BM'
'$Include:'Resources\Includes\movie-open.ico.BM'

Function WindowProc%& (hwnd As Offset, uMsg As Unsigned Long, wParam As Unsigned Offset, lParam As Offset)
    Const MAX_PATH = 65534
    Select Case uMsg
        Case WM_CREATE
        Case WM_CLOSE
            FadeOut parentWin, 500
            WindowProc = DefWindowProc(hwnd, uMsg, wParam, lParam)
            Exit Case
        Case WM_DESTROY
            PostQuitMessage 0
            WindowProc = 0
            Exit Function
        Case WM_TIMER
            Select Case wParam
                Case 1
                    If isPlaying = "playing" Then
                        RedrawWindow parentWin, 0, 0, &H0001 Or &H0100
                        SetTrackBarVal trackbar, Round((GetVideoPos * 100) / (GetVideoLength))
                    End If
            End Select
        Case WM_COMMAND
            Select Case lParam
                Case playpauseBtn
                    If isPlaying = "playing" Then
                        qPause
                    ElseIf isPlaying = "paused" Then
                        qResume
                    End If
                    Exit Case
                Case seekLbtn
                    SeekLeft
                    Exit Case
                Case seekRbtn
                    SeekRight
                    Exit Case
            End Select
            Select Case LOWORD(Val(Str$(wParam)))
                Case AboutMenu
                    Print LOWORD(Val(Str$(wParam))), LOWORD(Val(Str$(lParam)))
                    AboutPopup
                    Exit Case
                Case AboutMenu + 1
                    Dim As Offset video: video = ComDlgFileName("Select Video", Dir$("videos"), "Video Files (*.AVI, *.MP4, *.MKV, *.MPG, *.WMV)|*.AVI;*.MP4;*.MKV;*.MPG;*.WMV", 2, UNICODE)
                    If video <> 0 Then
                        wprintf video
                        If isPlaying = "playing" Or isPlaying = "paused" Then
                            AlreadyPlayingPopup PointerToWideString(video)
                        Else
                            If video <> 0 Then
                                qPlay PointerToWideString(video)
                                videoWin = VideoHandle
                                Print videoWin
                                AdjustWindow (videoWin)
                            End If
                        End If
                    End If
                    Exit Case
            End Select
            Exit Case
        Case WM_HSCROLL
            Print "Scrolled"
            Print GetTrackBarVal
            SetVideoPos
            Exit Case
        Case WM_SIZE
            Select Case wParam
                Case 2, 0
                    Print WindowWidth(parentWin), WindowHeight(parentWin)
                    MoveWindow playpauseBtn, (WindowWidth(parentWin) / 2) - 20, WindowHeight(parentWin) - 80, 40, 30, -1
                    MoveWindow seekLbtn, (WindowWidth(parentWin) / 2) - 60, WindowHeight(parentWin) - 80, 40, 30, -1
                    MoveWindow seekRbtn, (WindowWidth(parentWin) / 2) + 20, WindowHeight(parentWin) - 80, 40, 30, -1
                    MoveWindow trackbar, 5, WindowHeight(parentWin) - 110, WindowWidth(parentWin) - 20, 30, -1
                    If videoWin Then
                        If WindowWidth(parentWin) = 1280 And WindowHeight(parentWin) = 900 Then
                            SizeVideoWindow 1280, 720
                        Else
                            SizeVideoWindow WindowWidth(parentWin) - 100, WindowHeight(parentWin) - 100
                        End If
                    End If
            End Select
        Case MM_MCINOTIFY
            Select Case wParam
                Case MCI_NOTIFY_ABORTED
                    If isPlaying = "stopped" Then qStop
                    Exit Case
                Case MCI_NOTIFY_FAILURE
                    qStop
                    Exit Case
                Case MCI_NOTIFY_SUCCESSFUL
                    If isPlaying = "stopped" Then qStop
                    Exit Case
                Case MCI_NOTIFY_SUPERSEDED
                    Exit Case
            End Select
            If isPlaying = "playing" And GetVideoPos = GetVideoLength Then qStop
        Case Else
            'If isPlaying = "playing" Then
            '    SetTrackBarVal trackbar, Round((GetVideoPos * 100) / (GetVideoLength))
            'End If
            WindowProc = DefWindowProc(hwnd, uMsg, wParam, lParam)
    End Select
End Function

Function TaskDlgCallback%& (hwnd As Offset, msg As Unsigned Long, wParam As Unsigned Offset, lParam As Offset, lpRefData As Offset)
    Select Case msg
        Case TDN_BUTTON_CLICKED
            Select Case wParam
                Case TDCBF_OK_BUTTON
                    Exit Function
            End Select
        Case TDN_HYPERLINK_CLICKED 'Pointer to string containing hyperlink is stored in lParam
            If lParam Then 'checking that the value isn't 0
                Dim As String comm: comm = ANSIToUnicode("open" + Chr$(0))
                ShellExecuteW 0, Offset(comm), lParam, 0, 0, SW_SHOWNORMAL
            End If
    End Select
End Function

Sub ToggleEnable (controlHandle As Offset, __toggle As Byte)
    Select Case __toggle
        Case 1
            EnableWindow controlHandle, -1
        Case 0
            EnableWindow controlHandle, 0
    End Select
End Sub

Function OpenMediaFile~& (fileName As String)
    ClosePlayer
    Dim As Unsigned Long mciError
    Dim As String playCommand: playCommand = ANSIToUnicode("Open " + Chr$(34)) + fileName + ANSIToUnicode(Chr$(34) + " type mpegvideo alias movie parent " + Trim$(Str$(parentWin)) + " style child" + Chr$(0))
    mciError = mciSendStringW(playCommand, 0, 0, 0)
    Print mciError
    If mciError <> 0 Then
        Dim As String errString: errString = String$(128 * 2, Chr$(0))
        mciGetErrorStringW mciError, Offset(errString), 128 * 2
        ErrorPopup "MCI Error Encountered", errString + ANSIToUnicode(Chr$(0)), "Most MCI errors are caused by not having the proper codec installed. To fix this, please install the K-Lite Codec Pack Full by clicking this link:" + Chr$(10) + "<A HREF=" + Chr$(34) + "https://codecguide.com/download_k-lite_codec_pack_full.htm" + Chr$(34) + ">K-Lite Codec Pack Full Download</A>"
        isOpen = 0
    Else
        isOpen = -1
    End If
    OpenMediaFile = mciError
End Function

Sub qPlay (fileName As String)
    Dim As String ttitle: ttitle = ANSIToUnicode("Playing - ") + fileName + Chr$(0) + Chr$(0)
    SetWindowTextW parentWin, Offset(ttitle)
    If OpenMediaFile(fileName) = 0 Then
        If WindowWidth(parentWin) = 1280 And WindowHeight(parentWin) = 900 Then
            SizeVideoWindow 1280, 720
        Else
            SizeVideoWindow WindowWidth(parentWin) - 100, WindowHeight(parentWin) - 100
        End If
        PlayMediaFile
    Else
        qStop
    End If
End Sub

Sub PlayMediaFile ()
    If isOpen Then
        Dim As String playCommand: playCommand = "Play movie notify" + Chr$(0)
        mciSendStringA playCommand, 0, 0, parentWin
        playCommand = "set movie time format milliseconds" + Chr$(0)
        mciSendStringA playCommand, 0, 0, 0
    End If
End Sub

Function isPlaying$ ()
    Dim As String status: status = Space$(128)
    Dim As String query: query = "status movie mode" + Chr$(0)
    mciSendStringA query, Offset(status), 128, 0
    isPlaying = Mid$(status, 1, InStr(status, Chr$(0)) - 1)
End Function

Function VideoHandle%& ()
    Dim As String status: status = Space$(128)
    Dim As String query: query = "status movie window handle" + Chr$(0)
    mciSendStringA query, Offset(status), 128, 0
    VideoHandle = Val(Mid$(status, 1, InStr(status, Chr$(0)) - 1))
End Function

Sub qPause ()
    Dim As String playCommand: playCommand = ANSIToUnicode("Pause movie" + Chr$(0))
    mciSendStringW playCommand, 0, 0, 0
End Sub

Sub qResume ()
    Dim As String playCommand: playCommand = ANSIToUnicode("Resume movie" + Chr$(0))
    mciSendStringW playCommand, 0, 0, 0
End Sub

Sub SeekLeft ()
    Dim As Unsigned Integer64 position: position = GetVideoPos
    Dim As String playCommand
    If position < 10000 Then
        playCommand = "seek movie to start notify" + Chr$(0)
    Else
        position = position - 10000
        playCommand = "seek movie to " + Str$(position) + " notify" + Chr$(0)
    End If
    mciSendStringA playCommand, 0, 0, parentWin
    playCommand = "Play movie notify" + Chr$(0)
    mciSendStringA playCommand, 0, 0, parentWin
End Sub

Sub SeekRight ()
    Dim As Unsigned Integer64 position: position = GetVideoPos + 30000
    Dim As String playCommand
    If position < GetVideoLength Then
        playCommand = "seek movie to" + Str$(position) + " notify" + Chr$(0)
        mciSendStringA playCommand, 0, 0, parentWin
        playCommand = "Play movie notify" + Chr$(0)
        mciSendStringA playCommand, 0, 0, parentWin
    End If
End Sub

Sub SetVideoPos ()
    Dim As Unsigned Integer64 position: position = GetVideoLength * (GetTrackBarVal / 100)
    Dim As String playCommand
    If position < GetVideoLength Then
        playCommand = "seek movie to" + Str$(position) + " notify" + Chr$(0)
        mciSendStringA playCommand, 0, 0, parentWin
        playCommand = "Play movie notify" + Chr$(0)
        mciSendStringA playCommand, 0, 0, parentWin
    End If
End Sub

Sub qStop ()
    ClosePlayer
    Dim As String ttitle: ttitle = "QB64 Win32 Video Player" + Chr$(0)
    SetWindowTextA parentWin, Offset(ttitle)
    ToggleEnable playpauseBtn, 0
    ToggleEnable seekLbtn, 0
    ToggleEnable seekRbtn, 0
    ToggleEnable trackbar, 0
    SetTrackBarVal trackbar, 0
End Sub

Sub ClosePlayer ()
    If isOpen Then
        Dim As String playCommand: playCommand = "Close movie notify" + Chr$(0)
        mciSendStringA playCommand, 0, 0, parentWin
        isOpen = 0
    End If
End Sub

Sub AboutPopup
    Print "About"

    Dim As Offset hr

    Dim As TASKDIALOGCONFIG tdconfig

    tdconfig.cbSize = Len(tdconfig)
    tdconfig.hwndParent = parentWin
    tdconfig.dwFlags = TDF_CALLBACK_TIMER Or TDF_POSITION_RELATIVE_TO_WINDOW Or TDF_ENABLE_HYPERLINKS

    Dim As String szTitle: szTitle = ANSIToUnicode("About" + Chr$(0))
    tdconfig.pszWindowTitle = Offset(szTitle)
    tdconfig.pszMainIcon = TD_INFORMATION_ICON
    tdconfig.dwCommonButtons = TDCBF_OK_BUTTON
    Dim As String szHeader: szHeader = ANSIToUnicode("QB64 Win32 Video Player (Unicode)" + Chr$(0))
    tdconfig.pszMainInstruction = Offset(szHeader)
    Dim As String szBodyText: szBodyText = ANSIToUnicode("This program is still in its early stages. I am looking to add new features to it each and every day. Please download the <A HREF=" + Chr$(34) + "https://codecguide.com/download_k-lite_codec_pack_full.htm" + Chr$(34) + ">K-Lite Codec Pack (Full)</A> to get the latest codecs so that you may fully enjoy this program." + Chr$(0))
    tdconfig.pszContent = Offset(szBodyText)

    Dim As String szFooter: szFooter = ANSIToUnicode("Visit my <A HREF=" + Chr$(34) + "https://github.com/SpriggsySpriggs/QB64-Win32-Video-Player" + Chr$(34) + ">GitHub repo</A> for this project" + Chr$(0))
    tdconfig.pszFooter = Offset(szFooter)

    tdconfig.nDefaultButton = IDYES
    tdconfig.pfCallback = TaskDialogCallback

    hr = TaskDialogIndirect(Offset(tdconfig), 0, 0, 0)
End Sub

Sub AlreadyPlayingPopup (filename As String)
    Dim As Offset hr

    Dim As TASKDIALOGCONFIG tdconfig
    Dim As TASKDIALOG_BUTTON tdbtns(0 To 1)
    Dim As Long nButtonID

    tdbtns(0).nButtonID = TDCBF_YES_BUTTON
    tdbtns(1).nButtonID = TDCBF_CANCEL_BUTTON

    Dim As String tdbtn1Text: tdbtn1Text = ANSIToUnicode("Play new file" + Chr$(0))
    Dim As String tdbtn2Text: tdbtn2Text = ANSIToUnicode("Cancel" + Chr$(0))
    tdbtns(0).pszButtonText = Offset(tdbtn1Text)
    tdbtns(1).pszButtonText = Offset(tdbtn2Text)

    tdconfig.cbSize = Len(tdconfig)
    tdconfig.hwndParent = parentWin
    tdconfig.dwFlags = TDF_CALLBACK_TIMER Or TDF_POSITION_RELATIVE_TO_WINDOW Or TDF_ENABLE_HYPERLINKS

    Dim As String szTitle: szTitle = ANSIToUnicode("New File Detected" + Chr$(0))
    tdconfig.pszWindowTitle = Offset(szTitle)
    tdconfig.pszMainIcon = TD_INFORMATION_ICON
    Dim As String szHeader: szHeader = ANSIToUnicode("A new file has been loaded" + Chr$(0))
    tdconfig.pszMainInstruction = Offset(szHeader)
    Dim As String szBodyText: szBodyText = ANSIToUnicode("Do you wish to play the new file (" + Chr$(34)) + filename + ANSIToUnicode(Chr$(34) + ") or cancel?" + Chr$(0))
    tdconfig.pszContent = Offset(szBodyText)

    Dim As String szFooter: szFooter = ANSIToUnicode("Visit my <A HREF=" + Chr$(34) + "https://github.com/SpriggsySpriggs/QB64-Win32-Video-Player" + Chr$(34) + ">GitHub repo</A> for this project" + Chr$(0))
    tdconfig.pszFooter = Offset(szFooter)

    tdconfig.cButtons = 2
    tdconfig.pButtons = Offset(tdbtns())
    tdconfig.nDefaultButton = IDYES
    tdconfig.pfCallback = TaskDialogCallback

    hr = TaskDialogIndirect(Offset(tdconfig), Offset(nButtonID), 0, 0)

    If SUCCEEDED(hr) And nButtonID = TDCBF_YES_BUTTON Then
        qPlay filename
        videoWin = VideoHandle
        ToggleEnable playpauseBtn, 1
        ToggleEnable seekLbtn, 1
        ToggleEnable seekRbtn, 1
    End If
End Sub

Sub ErrorPopup (eTitle As String, mciErrString As String, errMessage As String)
    Print "ErrorPopup"

    Dim As TASKDIALOGCONFIG tdconfig
    tdconfig.dwCommonButtons = TDCBF_OK_BUTTON
    Dim As String tdbtntext: tdbtntext = ANSIToUnicode("OK" + Chr$(0))

    tdconfig.cbSize = Len(tdconfig)
    tdconfig.hwndParent = parentWin
    tdconfig.dwFlags = TDF_ENABLE_HYPERLINKS Or TDF_CALLBACK_TIMER Or TDF_POSITION_RELATIVE_TO_WINDOW

    Dim As String szTitle: szTitle = ANSIToUnicode(eTitle + Chr$(0))
    tdconfig.pszWindowTitle = Offset(szTitle)
    tdconfig.pszMainIcon = TD_ERROR_ICON
    Dim As String szHeader: szHeader = mciErrString
    tdconfig.pszMainInstruction = Offset(szHeader)
    Dim As String szBodyText: szBodyText = ANSIToUnicode(errMessage + Chr$(0))
    tdconfig.pszContent = Offset(szBodyText)
    tdconfig.nDefaultButton = IDOK

    Dim As String szFooter: szFooter = ANSIToUnicode("Visit my <A HREF=" + Chr$(34) + "https://github.com/SpriggsySpriggs/QB64-Win32-Video-Player" + Chr$(34) + ">GitHub repo</A> for this project" + Chr$(0))
    tdconfig.pszFooter = Offset(szFooter)

    tdconfig.pfCallback = TaskDialogCallback

    TaskDialogIndirect Offset(tdconfig), 0, 0, 0
End Sub

Function NewEditBox%& (x As Long, y As Long, w As Long, h As Long, style As String, defaultText As String)
    Const ES_LEFT = 0
    Const ES_MULTILINE = 4
    Const ES_AUTOVSCROLL = &H0040
    Const ES_AUTOHSCROLL = &H0080
    Const ES_WANTRETURN = &H1000
    Const ES_READONLY = &H0800
    Const WS_VSCROLL = &H00200000
    Const WS_HSCROLL = &H00100000

    Dim As Long flags
    style = UCase$(style)
    flags = WS_CHILD Or WS_VISIBLE
    If InStr(style, "NOWRAP") Then
        flags = flags Or WS_VSCROLL Or WS_HSCROLL Or ES_AUTOHSCROLL Or ES_AUTOVSCROLL Or ES_LEFT Or ES_MULTILINE Or ES_WANTRETURN
    ElseIf InStr(style, "WRAP") Then
        flags = flags Or WS_VSCROLL Or ES_AUTOVSCROLL Or ES_LEFT Or ES_MULTILINE Or ES_WANTRETURN
    Else
        flags = flags Or ES_LEFT
    End If
    If InStr(style, "READONLY") Then
        flags = flags Or ES_READONLY
    End If
    Dim As String class: class = WC_EDIT + Chr$(0)
    defaultText = defaultText + Chr$(0)
    NewEditBox = CreateWindowEx(0, Offset(class), Offset(defaultText), flags, x, y, w, h, parentWin, 0, hInstance, 0)
End Function

Sub SetFont (controlHandle As Offset, fontName As String * 32, __height As Long, style As String, weightStyle As String)
    Dim As Offset __font
    Dim As Byte bold, underline, strikeout, italic
    Dim As Long weight
    style = UCase$(style)
    If InStr(style, "UNDERLINE") Then underline = -1
    If InStr(style, "STRIKEOUT") Then strikeout = -1
    If InStr(style, "ITALIC") Then italic = -1
    Select Case UCase$(weightStyle)
        Case "THIN"
            weight = 100
        Case "EXTRALIGHT", "ULTRALIGHT"
            weight = 200
        Case "LIGHT"
            weight = 300
        Case "NORMAL", "REGULAR"
            weight = 400
        Case "MEDIUM"
            weight = 500
        Case "SEMIBOLD", "DEMIBOLD"
            weight = 600
        Case "BOLD"
            weight = 700
        Case "EXTRABOLD", "ULTRABOLD"
            weight = 800
        Case "HEAVY", "BLACK"
            weight = 900
        Case Else
            weight = 0
    End Select
    Dim As Long FF_DECORATIVE: FF_DECORATIVE = _SHL(5, 4)
    Dim As Long FF_MODERN: FF_MODERN = _SHL(3, 4)
    Dim As Long FF_ROMAN: FF_ROMAN = _SHL(1, 4)
    Dim As Long FF_SCRIPT: FF_SCRIPT = _SHL(4, 4)
    Dim As Long FF_SWISS: FF_SWISS = _SHL(2, 4)
    __font = CreateFont(__height, 0, 0, 0, weight, italic, underline, strikeout, 0, 0, 0, 5, FF_DECORATIVE Or FF_MODERN Or FF_ROMAN Or FF_SCRIPT Or FF_SWISS, fontName + Chr$(0))
    If Font Then
        SendMessage controlHandle, WM_SETFONT, __font, MAKELPARAM(-1, 0)
    End If
End Sub

Sub SetTrackBarVal (tbHandle As Offset, newValue As Unsigned Integer64)
    SendMessage tbHandle, TBM_SETPOS, -61, newValue
End Sub

Function GetTrackBarVal~&
    Const TBM_GETPOS = &H0400
    GetTrackBarVal = Val(Str$(SendMessage(trackbar, TBM_GETPOS, 0, 0)))
End Function

Function CommandW%& ()
    Declare CustomType Library
        Function GetCommandLineW%& ()
        Function GetModuleFileNameW~& (ByVal hModule As Offset, Byval lpFilename As Offset, Byval nSize As Unsigned Long)
    End Declare
    Const MAX_PATH = 65534
    Dim As String * MAX_PATH szFilename
    Dim As Unsigned Long execlength: execlength = GetModuleFileNameW(0, Offset(szFilename), MAX_PATH)
    If execlength Then
        Dim As Offset commandline: commandline = GetCommandLineW
        commandline = commandline + (execlength * 2) + 6
        TrimW commandline, Chr$(34)
        CommandW = commandline
    End If
End Function

Declare Dynamic Library "Shlwapi"
    Function StrStrW%& (ByVal pszFirst As _Offset, Byval pszSrch As _Offset)
    Sub StrTrimW (ByVal psz As _Offset, Byval pszTrimChars As _Offset)
    Function StrRStrIW%& (ByVal pszSource As Offset, Byval pszLast As Offset, Byval pszSearch As Offset)
End Declare

Sub TrimW (stringToTrim As _Offset, toTrim As String)
    toTrim = ANSIToUnicode(toTrim + Chr$(0))
    StrTrimW stringToTrim, _Offset(toTrim)
End Sub

Function FileExistsW%% (filename As _Offset)
    Declare CustomType Library
        Function GetFileAttributesW~& (ByVal lpFileName As _Offset)
    End Declare
    Const INVALID_FILE_ATTRIBUTES = -1~&
    Const FILE_ATTRIBUTE_DIRECTORY = &H00000010
    Dim As _Unsigned Long dwAttrib: dwAttrib = GetFileAttributesW(filename)
    FileExistsW = (dwAttrib <> INVALID_FILE_ATTRIBUTES And Not (dwAttrib And FILE_ATTRIBUTE_DIRECTORY))
End Function

Sub ChangeIcon (controlHandle As Offset, iconFile As String)
    Const BM_SETIMAGE = &H00F7
    Const IMAGE_ICON = 1
    SendMessage controlHandle, BM_SETIMAGE, IMAGE_ICON, ExtractIcon(0, CWD$ + "\" + iconFile + Chr$(0), 0)
End Sub

Function CheckExtension%% (fileName As Offset)
    Declare CustomType Library
        Function CharUpperW%& (ByVal lpsz As Offset)
    End Declare
    Dim As String avi, mp4, mkv, mpg, wmv
    avi = ANSIToUnicode(".AVI"): mp4 = ANSIToUnicode(".MP4"): mkv = ANSIToUnicode(".MKV"): mpg = ANSIToUnicode(".MPG"): wmv = ANSIToUnicode(".WMV")
    Dim As String decimal: decimal = ANSIToUnicode("." + Chr$(0))
    Dim As Offset pExtension: pExtension = StrRStrIW(fileName, 0, Offset(decimal))
    Dim As String extension: extension = PointerToWideString(CharUpperW(pExtension))
    If extension <> avi And extension <> mp4 And extension <> mkv And extension <> mpg And extension <> wmv Then
        CheckExtension = 0
    Else
        CheckExtension = -1
    End If
End Function

Function WindowHeight& (win As Offset)
    Dim As RECT rect
    GetWindowRect win, Offset(rect)
    WindowHeight = rect.bottom - rect.top
End Function

Function WindowWidth& (win As Offset)
    Dim As RECT rect
    GetWindowRect win, Offset(rect)
    WindowWidth = rect.right - rect.left
End Function

Sub qMoveWindow (X As Long, Y As Long, win As Offset)
    Const SWP_NOSIZE = &H0001
    SetWindowPos win, 0, X, Y, 0, 0, SWP_NOSIZE
End Sub

Sub AdjustWindow (vhandle As Offset)
    ToggleEnable playpauseBtn, 1
    ToggleEnable seekLbtn, 1
    ToggleEnable seekRbtn, 1
    ToggleEnable trackbar, 1
End Sub

Sub SizeVideoWindow (maxX As Unsigned Long, maxY As Unsigned Long)
    Dim As String size: size = GetVideoSize
    Dim As String rc(0 To 3)
    tokenize size, " ", rc()
    Dim As Unsigned Long videoX, videoY
    rc(2) = rc(2)
    rc(3) = rc(3)
    videoX = Val(rc(2))
    videoY = Val(rc(3))
    Dim As Single AspectRatio: AspectRatio = videoX / videoY
    Dim As Long iLeft, iTop
    Dim As Long newWidth: newWidth = maxX
    Dim As Long newHeight: newHeight = newWidth \ AspectRatio

    If newHeight > maxY Then
        newHeight = maxY
        newWidth = newHeight * AspectRatio
        iLeft = (maxX - newWidth) \ 2
    Else
        iTop = (maxY - newHeight) \ 2
    End If
    Dim As String res
    If WindowWidth(parentWin) = 1280 And WindowHeight(parentWin) = 900 Then
        res = "put movie window at 0 30 1280 720" + Chr$(0)
    Else
        If videoX < maxX And videoY < maxY Then
            res = "put movie window at 0 30 1920 817" + Chr$(0)
        Else
            Print newWidth, newHeight, AspectRatio
            Print videoX, videoY
            res = "put movie window at" + Str$(iLeft + 125) + Str$(iTop - 10) + Str$(newWidth - 175) + Str$((newHeight - 175) + GetTitlebarSize * 4) + Chr$(0)
        End If
    End If
    mciSendStringA res, 0, 0, 0
End Sub

Function GetTitlebarSize~& ()
    Dim As POINT pt
    Dim As RECT rc
    ClientToScreen parentWin, Offset(pt)
    GetWindowRect parentWin, Offset(rc)
    GetTitlebarSize = pt.y - rc.top
End Function

Sub CenterWindow (win As Offset)
    Dim As Long nX, nY
    nX = (DesktopWidth / 2) - (WindowWidth(win) / 2)
    nY = (DesktopHeight / 2) - (WindowHeight(win) / 2)
    qMoveWindow nX, nY, win
End Sub

Function GetVideoLength~&& ()
    Dim As String length: length = Space$(128 * 2)
    Dim As String query: query = "status movie length" + Chr$(0)
    mciSendStringA query, Offset(length), 128, 0
    GetVideoLength = Val(length)
End Function

Function GetVideoSize$ ()
    Dim As String size: size = Space$(128)
    Dim As String query: query = "where movie source" + Chr$(0)
    mciSendStringA query, Offset(size), 128, 0
    GetVideoSize = Mid$(size, 1, InStr(size, Chr$(0)) - 1)
End Function

Function GetVideoPos~&&
    Dim As String position: position = Space$(128)
    Dim As String query: query = "status movie position" + Chr$(0)
    mciSendStringA query, Offset(position), 128, 0
    GetVideoPos = Val(position)
End Function

Sub FadeOut (handle As Offset, time As Unsigned Long) 'Doesn't stop the program. Only hides it
    AnimateWindow handle, time, AW_BLEND Or AW_HIDE
End Sub

Sub FadeIn (handle As Offset, time As Unsigned Long) 'Only use after a function that hides the window
    AnimateWindow handle, time, AW_BLEND Or AW_ACTIVATE
End Sub

Sub SetConsoleUnicode ()
    Const CP_UTF8 = 65001
    Const LC_ALL = 0

    Declare CustomType Library
        Sub wprintf (ByVal format As _Offset)
        Sub printf (ByVal format As _Offset)
        Sub SetConsoleCP (ByVal wCodePageID As Unsigned Long)
        Sub SetConsoleOutputCP (ByVal wCodePageID As Unsigned Long)
        Function setlocale%& Alias "_wsetlocale" (ByVal category As Long, Byval locale As Offset)
    End Declare

    SetConsoleCP CP_UTF8
    SetConsoleOutputCP CP_UTF8

    Dim As String newLocale: newLocale = ANSIToUnicode(".<UTF-8>" + Chr$(0))
    Dim As Offset locale: locale = setlocale(LC_ALL, Offset(newLocale))
End Sub

Sub tokenize (toTokenize As String, delimiters As String, StorageArray() As String)
    Declare CustomType Library
        Function strtok%& (ByVal str As Offset, delimiters As String)
    End Declare
    Dim As Offset tokenized
    Dim As String tokCopy: tokCopy = toTokenize + Chr$(0)
    Dim As String delCopy: delCopy = delimiters + Chr$(0)
    Dim As Unsigned Long lowerbound: lowerbound = LBound(StorageArray)
    Dim As Unsigned Long i: i = lowerbound
    tokenized = strtok(Offset(tokCopy), delCopy)
    While tokenized <> 0
        ReDim Preserve StorageArray(lowerbound To UBound(StorageArray) + 1)
        StorageArray(i) = PointerToString(tokenized)
        tokenized = strtok(0, delCopy)
        i = i + 1
    Wend
    ReDim Preserve StorageArray(UBound(StorageArray) - 1)
End Sub

'$Include:'OpenSaveW.BM'
'$Include:'ptrtostr.BM'
