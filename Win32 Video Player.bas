Option Explicit
$NoPrefix

$VersionInfo:CompanyName=SpriggsySpriggs
$VersionInfo:FileDescription=QB64 Video Player
$VersionInfo:ProductName=QB64 Video Player

$Console:Only
Console Off

$ExeIcon:'Video Player icons\video.ico'
Icon
'$Let UNICODE = DEFINED
'$Include:'OpenSave.BI'
'$Include:'TaskDlg.BI'

Const DEFEXT = "MP4"

Const MM_MCINOTIFY = &H3B9
Const MCI_NOTIFY_SUCCESSFUL = &H0001
Const MCI_NOTIFY_SUPERSEDED = &H0002
Const MCI_NOTIFY_ABORTED = &H0004
Const MCI_NOTIFY_FAILURE = &H0008

Const IDI_APPLICATION = 32512
Const IDC_ARROW = 32512
Const COLOR_WINDOW = 5

Const WS_EX_CLIENTEDGE = &H00000200
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

Const WS_VSCROLL = &H00200000
Const WS_HSCROLL = &H00100000

Const BS_PUSHBUTTON = 0
Const BS_AUTOCHECKBOX = 3
Const BS_GROUPBOX = 7
Const BS_AUTORADIOBUTTON = 9
Const BS_TEXT = 0
Const BS_SPLITBUTTON = &HC
Const BS_DEFPUSHBUTTON = &H00000001

Const SW_SHOWDEFAULT = &HA

Const WM_DESTROY = 2
Const WM_GETTEXT = &H000D
Const WM_CLOSE = &H0010
Const WM_COMMAND = &H0111
Const WM_ENTERSIZEMOVE = &H0231
Const WM_EXITSIZEMOVE = &H0232
Const WM_CREATE = &H0001
Const WM_TIMER = &H0113
Const WM_DROPFILES = &H0233

Const IDCANCEL = 2
Const IDNO = 7
Const IDOK = 1
Const IDRETRY = 4
Const IDYES = 6

Const LF_FACESIZE = 32

Const SPI_GETNONCLIENTMETRICS = &H0029
Const WM_SETFONT = &H0030

Const BM_GETCHECK = &HF0

Const WM_USER = &H0400

Const COMCTL32_VERSION = 6

Const TRACKBAR_CLASS = "msctls_trackbar32"

Const WC_EDIT = "Edit"
Const WM_SETTEXT = &H000C

Const ES_LEFT = 0
Const ES_MULTILINE = 4
Const ES_AUTOVSCROLL = &H0040
Const ES_AUTOHSCROLL = &H0080
Const ES_WANTRETURN = &H1000
Const ES_READONLY = &H0800

Const WM_ENABLE = &H000A
Const WM_CTLCOLORBTN = &H0135

Const BS_BITMAP = &H00000080

Const TBM_SETRANGE = WM_USER + 6
Const TBM_SETPOS = WM_USER + 5

Const SW_SHOWNORMAL = 1

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
    Function SendMessage%& (ByVal hWnd As Offset, Byval Msg As Unsigned Long, Byval wParam As Unsigned Offset, Byval lParam As Offset)
    Sub SendMessage (ByVal hWnd As Offset, Byval Msg As Unsigned Long, Byval wParam As Unsigned Offset, Byval lParam As Offset)
    Function DefWindowProc%& (ByVal hWnd As Offset, Byval Msg As Unsigned Long, Byval wParam As Unsigned Offset, Byval lParam As Offset)
    Sub PostQuitMessage (ByVal nExitCode As Long)
    Function LoadCursor%& (ByVal hInstance As Offset, Byval lpCursorName As Offset)
    Function LoadIcon%& (ByVal hInstance As Offset, Byval lpIconName As Offset)
    Function RegisterClassEx% (ByVal wndclassex As Offset)
    Function CreateWindowEx%& (ByVal dwExStyle As Long, Byval lpClassName As Offset, Byval lpWindowName As Offset, Byval dwStyle As Long, Byval X As Long, Byval Y As Long, Byval nWidth As Long, Byval nHeight As Long, Byval hWndParent As Offset, Byval wMenu As Offset, Byval hInstance As Offset, Byval lpParam As Offset)
    Sub CreateWindowEx (ByVal dwExStyle As Long, Byval lpClassName As Offset, Byval lpWindowName As Offset, Byval dwStyle As Long, Byval X As Long, Byval Y As Long, Byval nWidth As Long, Byval nHeight As Long, Byval hWndParent As Offset, Byval wMenu As Offset, Byval hInstance As Offset, Byval lpParam As Offset)
    Sub ShowWindow (ByVal hWnd As Offset, Byval nCmdShow As Long)
    Sub UpdateWindow (ByVal hWnd As Offset)
    Function GetMessage%% (ByVal lpMsg As Offset, Byval hWnd As Offset, Byval wMsgFilterMin As Unsigned Long, Byval wMsgFilterMax As Unsigned Long)
    Sub TranslateMessage (ByVal lpMsg As Offset)
    Sub DispatchMessage (ByVal lpMsg As Offset)
    Function GetModuleHandle%& (ByVal lpModulename As Offset)
    Sub SystemParametersInfo (ByVal uiAction As Unsigned Long, Byval uiParam As Unsigned Long, Byval pvParam As Offset, Byval fWinIni As Unsigned Long)
    Sub SetWindowPos (ByVal hWnd As Offset, Byval hWndInsertAfter As Offset, Byval X As Long, Byval Y As Long, Byval cx As Long, Byval cy As Long, Byval uFlags As Unsigned Long)
    Sub EndDialog (ByVal hDlg As Offset, Byval nResult As Offset)
    Sub EnableWindow (ByVal hWnd As Offset, Byval bEnable As Long)
    Sub GetWindowRect (ByVal hWnd As Offset, Byval lpRect As Offset)
    Function GetDC%& (ByVal hWnd As Offset)
    Function GetDlgItem%& (ByVal hDlg As Offset, Byval nIDDlgItem As Long)
    Function GetDlgCtrlID& (ByVal hWnd As Offset)
    Sub GetWindowText (ByVal hWnd As Offset, Byval lpString As Offset, Byval nMaxCount As Long)
    Sub SetWindowText (ByVal hWnd As Offset, Byval lpString As Offset)
    Function FindWindow%& (ByVal lpClassName As Offset, Byval lpWindowName As Offset)
    Function GetWindowLongPtr%& (ByVal hWnd As Offset, Byval nIndex As Long)
    Sub SetWindowLongPtr (ByVal hWnd As Offset, Byval nIndex As Long, Byval dwNewLong As Offset)
    Sub SetClassLongPtr (ByVal hWnd As Offset, Byval nIndex As Long, Byval dwNewLong As Offset)
    Sub RedrawWindow (ByVal hWnd As Offset, Byval lprcUpdate As Offset, Byval hrgnUpdate As Offset, Byval flags As Unsigned Long)
    Sub SetTimer (ByVal hWnd As Offset, Byval nIDEvent As Unsigned Offset, Byval uElapse As Unsigned Long, Byval lpTimerFunc As Offset)
    Sub ClientToScreen (ByVal hWnd As Offset, Byval lpPoint As Offset)
    Sub ShellExecuteW (ByVal hwnd As Offset, lpOperation As String, lpFile As String, Byval lpParameters As Offset, Byval lpDirectory As Offset, Byval nShowCmd As Long)
    Function GetLastError& ()
End Declare

Declare CustomType Library "makeint"
    Function MAKEINTRESOURCE%& Alias "MAKEINTRSC" (ByVal i As Offset)
End Declare

Declare Library
    Function MAKELPARAM%& (ByVal l As Integer, Byval h As Integer)
    Function __RGB~& Alias "RGB" (ByVal r As Unsigned Byte, Byval g As Unsigned Byte, Byval b As Unsigned Byte)
End Declare

Declare Dynamic Library "Gdi32"
    'Function CreateFontIndirect%& Alias "CreateFontIndirectA" (ByVal lplf As Offset)
    Function CreateFont%& Alias "CreateFontA" (ByVal cHeight As Long, Byval cWidth As Long, Byval cEscapement As Long, Byval cOrientation As Long, Byval cWeight As Long, Byval bItalic As Unsigned Long, Byval bUnderline As Unsigned Long, Byval bStrikeout As Unsigned Long, Byval iCharSet As Unsigned Long, Byval iOutPrecision As Unsigned Long, Byval iClipPrecision As Unsigned Long, Byval iQuality As Unsigned Long, Byval iPitchAndFamily As Unsigned Long, pszFaceName As String)
    Sub SetBkColor (ByVal hdc As Offset, Byval color As Unsigned Long)
    Sub SetDCBrushColor (ByVal hdc As Offset, Byval color As Unsigned Long)
End Declare

Declare CustomType Library ".\internal\c\c_compiler\x86_64-w64-mingw32\include\shellapi"
    Function ExtractIcon%& (ByVal hInst As Offset, pszExeFileName As String, Byval nIconIndex As Unsigned Long)
    Sub DragAcceptFiles (ByVal hWnd As Offset, Byval fAccept As Long)
    Sub DragQueryFile (ByVal hDrop As Offset, Byval iFile As Unsigned Long, Byval lpszFile As Offset, Byval cch As Unsigned Long)
    Sub DragFinish (ByVal hDrop As Offset)
End Declare

Declare Library "win"
    Function GetWindowProc%& ()
End Declare

Declare Dynamic Library "Winmm"
    Function mciSendString~& Alias "mciSendStringA" (ByVal lpszCommand As Offset, Byval lpszReturnString As Offset, Byval cchReturn As Unsigned Long, Byval hwndCallback As Offset)
    Sub mciSendString Alias "mciSendStringA" (ByVal lpszCommand As Offset, Byval lpszReturnString As Offset, Byval cchReturn As Unsigned Long, Byval hwndCallback As Offset)
    Sub mciGetErrorString Alias "mciGetErrorStringA" (ByVal fdwError As Unsigned Long, Byval lpszErrorText As Offset, Byval cchErrorText As Unsigned Long)
End Declare

Dim Shared As Byte isOpen: isOpen = 0
Dim Shared As Byte isMaxed: isMaxed = 0

Dim As WNDCLASSEX wc
Dim As String className: className = "main" + Chr$(0)
Dim Shared As Offset hInstance: hInstance = GetModuleHandle(0)
Dim Shared As Offset videoWin, deviceID
Dim As MSG msg

ChDir "Video Player icons"

wc.cbSize = Len(wc)
wc.lpfnWndProc = GetWindowProc
wc.hInstance = hInstance
wc.hCursor = LoadCursor(0, MAKEINTRESOURCE(IDC_ARROW))
wc.hIcon = ExtractIcon(0, CWD$ + "\video.ico" + Chr$(0), 0)
wc.hbrBackground = COLOR_WINDOW + 1
wc.lpszClassName = Offset(className)

Dim As Integer reg: reg = RegisterClassEx(Offset(wc))
If reg = 0 Then System

Dim As String parentTitle: parentTitle = "QB64 Win32 Video Player" + Chr$(0)
Dim Shared As Offset parentWin: parentWin = CreateWindowEx(0, MAKELPARAM(reg, 0), Offset(parentTitle), WS_OVERLAPPED Or WS_CAPTION Or WS_SYSMENU Or WS_MINIMIZEBOX Or WS_MAXIMIZEBOX, CW_USEDEFAULT, CW_USEDEFAULT, 320, 250, 0, 0, hInstance, 0)
If parentWin = 0 Then System

Dim As String t0: t0 = "BUTTON" + Chr$(0)
Dim As String t1: t1 = "Select video to play" + Chr$(0)
Dim Shared As Offset selectBtn
selectBtn = CreateWindowEx(0, Offset(t0), Offset(t1), WS_TABSTOP Or WS_VISIBLE Or WS_CHILD Or BS_DEFPUSHBUTTON, 10, 10, 295, 40, parentWin, 0, hInstance, 0)

Dim Shared As Offset pauseBtn
pauseBtn = CreateWindowEx(0, Offset(t0), 0, WS_TABSTOP Or WS_VISIBLE Or WS_CHILD Or BS_BITMAP, 140, 150, 40, 40, parentWin, 0, hInstance, 0)

Dim Shared As Offset rewindBtn
rewindBtn = CreateWindowEx(0, Offset(t0), 0, WS_TABSTOP Or WS_VISIBLE Or WS_CHILD Or BS_BITMAP, 100, 150, 40, 40, parentWin, 0, hInstance, 0)

Dim Shared As Offset ffBtn
ffBtn = CreateWindowEx(0, Offset(t0), 0, WS_TABSTOP Or WS_VISIBLE Or WS_CHILD Or BS_BITMAP, 180, 150, 40, 40, parentWin, 0, hInstance, 0)

Dim Shared As Offset text
text = NewEditBox(10, 110, 295, 16, "READONLY", "")

t0 = TRACKBAR_CLASS + Chr$(0)
Dim Shared As Offset trackbar
trackbar = CreateWindowEx(0, Offset(t0), 0, WS_CHILD Or WS_VISIBLE, 10, 80, 295, 20, parentWin, 0, hInstance, 0)

SetFont selectBtn, "Arial", 18, "", ""
SetFont text, "Arial", 16, "", ""

ChangeIcon selectBtn, "folder.ico"
ChangeIcon pauseBtn, "pause-button.ico"
ChangeIcon rewindBtn, "rewind-button.ico"
ChangeIcon ffBtn, "ff-button.ico"

ToggleEnable pauseBtn, 0
ToggleEnable rewindBtn, 0
ToggleEnable ffBtn, 0
ToggleEnable trackbar, 0

SendMessage trackbar, TBM_SETRANGE, 0, MAKELPARAM(0, 100)

ShowWindow parentWin, SW_SHOWDEFAULT
UpdateWindow parentWin

DragAcceptFiles parentWin, 1

If CommandCount > 0 Then
    Dim As String comf: comf = Command$(1)
    If FileExists(comf) Then
        Print comf
        SetWindowText text, Offset(comf)
        qPlay comf
        comf = comf + Chr$(0)
        videoWin = FindWindow(0, Offset(comf))
        CenterWindow videoWin
        LockWindowResize videoWin
        ToggleEnable pauseBtn, 1
        ToggleEnable rewindBtn, 1
        ToggleEnable ffBtn, 1
    End If
End If

SetTimer parentWin, 1, 500, 0

While GetMessage(Offset(msg), 0, 0, 0)
    TranslateMessage Offset(msg)
    DispatchMessage Offset(msg)
Wend

System Val(Str$(msg.wParam))

Function WindowProc%& (hwnd As Offset, uMsg As Unsigned Long, wParam As Unsigned Offset, lParam As Offset)
    Select Case uMsg
        Case WM_CREATE
            ToggleEnable pauseBtn, 0
        Case WM_CLOSE
            WindowProc = DefWindowProc(hwnd, uMsg, wParam, lParam)
            Exit Case
        Case WM_DESTROY
            PostQuitMessage 0
            WindowProc = 0
            Exit Function
        Case WM_DROPFILES
            Dim As Offset hDrop: hDrop = wParam
            Dim As String dropped: dropped = Space$(260)
            Dim As String videof
            DragQueryFile hDrop, 0, Offset(dropped), 260
            videof = Mid$(dropped, 1, InStr(dropped, Chr$(0)))
            Print hDrop, videof
            DragFinish hDrop
            If CheckExtension(Mid$(videof, 1, Len(videof) - 1)) And FileExists(Mid$(videof, 1, Len(videof) - 1)) Then
                If isPlaying = "playing" Or isPlaying = "paused" Then
                    AlreadyPlayingPopup Mid$(videof, 1, Len(videof) - 1)
                Else
                    SetWindowText text, Offset(videof)
                    qPlay Mid$(videof, 1, Len(videof) - 1)
                    videoWin = FindWindow(0, Offset(videof))
                    CenterWindow videoWin
                    LockWindowResize videoWin
                    ToggleEnable pauseBtn, 1
                    ToggleEnable rewindBtn, 1
                    ToggleEnable ffBtn, 1
                End If
            ElseIf CheckExtension(Mid$(videof, 1, Len(videof) - 1)) And FileExists(Mid$(videof, 1, Len(videof) - 1)) = 0 Then
                ErrorPopup "File Path Error", "File contains unsupported characters", "The file you have selected contains characters that this program is currently unable to handle. Please stay tuned as I might eventually add support for Unicode files."
            Else
                ErrorPopup "File Format Error", "Unsupported file format", "I haven't yet checked this file type. Perhaps it is a strange one you like. However, please stick to using the ones allowed by the open file dialog." + Chr$(10) + Chr$(10) + "-Spriggsy"
            End If
        Case WM_TIMER
            Select Case wParam
                Case 1
                    If isPlaying = "playing" Then
                        RedrawWindow parentWin, 0, 0, &H0001 Or &H0100
                    End If
            End Select
        Case WM_COMMAND
            Select Case lParam
                Case selectBtn
                    Dim As String video: video = ComDlgFileName("Select Video", Dir$("videos"), "Video Files (*.AVI, *.MP4, *.MKV, *.MPG, *.WMV)|*.AVI;*.MP4;*.MKV;*.MPG;*.WMV", 2, 0) + Chr$(0)
                    Print video, Len(video)
                    If Mid$(video, 1, Len(video) - 1) <> "" Then
                        If CheckExtension(Mid$(video, 1, Len(video) - 1)) And FileExists(Mid$(video, 1, Len(video) - 1)) Then
                            If isPlaying = "playing" Or isPlaying = "paused" Then
                                AlreadyPlayingPopup Mid$(video, 1, Len(video) - 1)
                            Else
                                Print Mid$(video, 1, Len(video) - 1)
                                SetWindowText text, Offset(video)
                                qPlay Mid$(video, 1, Len(video) - 1)
                                videoWin = FindWindow(0, Offset(video))
                                CenterWindow videoWin
                                LockWindowResize videoWin
                                ToggleEnable pauseBtn, 1
                                ToggleEnable rewindBtn, 1
                                ToggleEnable ffBtn, 1
                            End If
                        ElseIf CheckExtension(Mid$(video, 1, Len(video) - 1)) And FileExists(Mid$(video, 1, Len(video) - 1)) = 0 Then
                            Print "File path error"
                            ErrorPopup "File Path Error", "File contains unsupported characters", "The file you have selected contains characters that this program is currently unable to handle. Please stay tuned as I might eventually add support for Unicode files."
                        Else
                            ErrorPopup "File Format Error", "Unsupported file format", "I haven't yet checked this file type. Perhaps it is a strange one you like. However, please stick to using the ones allowed by the open file dialog." + Chr$(10) + Chr$(10) + "-Spriggsy"
                        End If
                    End If
                    Exit Case
                Case pauseBtn
                    If isPlaying = "playing" Then
                        qPause
                    ElseIf isPlaying = "paused" Then
                        qResume
                    End If
                    Exit Case
                Case rewindBtn
                    SeekLeft
                    Exit Case
                Case ffBtn
                    SeekRight
                    Exit Case
            End Select
        Case MM_MCINOTIFY
            Select Case wParam
                Case MCI_NOTIFY_ABORTED
                    Print "Aborted"
                    If isPlaying = "stopped" Then qStop
                    Exit Case
                Case MCI_NOTIFY_FAILURE
                    qStop
                    Print "Failed"
                    Exit Case
                Case MCI_NOTIFY_SUCCESSFUL
                    If isPlaying = "stopped" Then qStop
                    Print "Succeeded"
                    Exit Case
                Case MCI_NOTIFY_SUPERSEDED
                    Print "Superseded"
                    Exit Case
            End Select
            If isPlaying = "playing" And GetVideoPos = GetVideoLength Then qStop
            Exit Case
        Case Else
            If isPlaying = "playing" Then
                SetTrackBarVal trackbar, Round((GetVideoPos * 100) / (GetVideoLength))
            End If
            WindowProc = DefWindowProc(hwnd, uMsg, wParam, lParam)
    End Select
End Function

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
    Dim As String szHeader: szHeader = ANSIToUnicode(mciErrString + Chr$(0))
    tdconfig.pszMainInstruction = Offset(szHeader)
    Dim As String szBodyText: szBodyText = ANSIToUnicode(errMessage + Chr$(0))
    tdconfig.pszContent = Offset(szBodyText)
    tdconfig.nDefaultButton = IDOK

    tdconfig.pfCallback = TaskDialogCallback

    TaskDialogIndirect Offset(tdconfig), 0, 0, 0
End Sub

Sub AlreadyPlayingPopup (filename As String)
    Print "Already playing"

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
    tdconfig.dwFlags = TDF_CALLBACK_TIMER Or TDF_POSITION_RELATIVE_TO_WINDOW

    Dim As String szTitle: szTitle = ANSIToUnicode("New File Detected" + Chr$(0))
    tdconfig.pszWindowTitle = Offset(szTitle)
    tdconfig.pszMainIcon = TD_INFORMATION_ICON
    Dim As String szHeader: szHeader = ANSIToUnicode("A new file has been loaded" + Chr$(0))
    tdconfig.pszMainInstruction = Offset(szHeader)
    Dim As String szBodyText: szBodyText = ANSIToUnicode("Do you wish to play the new file " + Chr$(34) + filename + Chr$(34) + " or cancel?" + Chr$(0))
    tdconfig.pszContent = Offset(szBodyText)

    tdconfig.cButtons = 2
    tdconfig.pButtons = Offset(tdbtns())
    tdconfig.nDefaultButton = IDYES

    hr = TaskDialogIndirect(Offset(tdconfig), Offset(nButtonID), 0, 0)

    If SUCCEEDED(hr) And nButtonID = TDCBF_YES_BUTTON Then
        SetWindowText text, Offset(filename)
        qPlay filename
        filename = filename + Chr$(0)
        videoWin = FindWindow(0, Offset(filename))
        CenterWindow videoWin
        LockWindowResize videoWin
        ToggleEnable pauseBtn, 1
        ToggleEnable rewindBtn, 1
        ToggleEnable ffBtn, 1
    End If
End Sub

Function TaskDlgCallback%& (hwnd As _Offset, msg As _Unsigned Long, wParam As _Unsigned _Offset, lParam As _Offset, lpRefData As _Offset)
    Select Case msg
        Case TDN_BUTTON_CLICKED
            Select Case wParam
                Case TDCBF_OK_BUTTON
                    Exit Function
            End Select
        Case TDN_HYPERLINK_CLICKED 'Pointer to string containing hyperlink is stored in lParam
            If lParam Then 'checking that the value isn't 0
                Dim As String hyperlink: hyperlink = wCharPtrToString(lParam) 'converting the lParam pointer to an ANSI string
                ShellExecuteW 0, ANSIToUnicode("open" + Chr$(0)), hyperlink + Chr$(0), 0, 0, SW_SHOWNORMAL
            End If
    End Select
End Function

Function NewEditBox%& (x As Long, y As Long, w As Long, h As Long, style As String, defaultText As String)
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

Sub MoveWindow (X As Long, Y As Long, win As Offset)
    Const SWP_NOSIZE = &H0001
    SetWindowPos win, 0, X, Y, 0, 0, SWP_NOSIZE
End Sub

Sub ResizeWindow (w As Long, h As Long, win As Offset)
    Const SWP_NOMOVE = &H0002
    SetWindowPos win, 0, 0, 0, w, h, SWP_NOMOVE
End Sub

Sub CenterWindow (win As Offset)
    Dim As Long nX, nY
    nX = (DesktopWidth / 2) - (WindowWidth(win) / 2)
    nY = (DesktopHeight / 2) - (WindowHeight(win) / 2)
    MoveWindow nX, nY, win
End Sub

Function WindowTitle$ (win As Offset)
    Dim As String wintitle: wintitle = Space$(260)
    GetWindowText win, Offset(wintitle), Len(wintitle)
    WindowTitle = wintitle
End Function

Function GetVideoLength~&& ()
    Dim As String length: length = Space$(128)
    Dim As String query: query = "status movie length" + Chr$(0)
    mciSendString Offset(query), Offset(length), 128, 0
    GetVideoLength = Val(length)
End Function

Function GetVideoSize$ ()
    Dim As String size: size = Space$(128)
    Dim As String query: query = "where movie source" + Chr$(0)
    mciSendString Offset(query), Offset(size), 128, 0
    GetVideoSize = Mid$(size, 1, InStr(size, Chr$(0)) - 1)
End Function

Sub ChangeIcon (controlHandle As Offset, iconFile As String)
    Const BM_SETIMAGE = &H00F7
    Const IMAGE_ICON = 1
    SendMessage controlHandle, BM_SETIMAGE, IMAGE_ICON, ExtractIcon(0, CWD$ + "\" + iconFile + Chr$(0), 0)
End Sub

Sub LockWindowResize (win As Offset)
    Const GWL_STYLE = -16
    Dim As Offset windowStyle: windowStyle = GetWindowLongPtr(win, GWL_STYLE)
    If windowStyle <> 0 Then
        If windowStyle And WS_THICKFRAME Then
            windowStyle = windowStyle - WS_THICKFRAME
            SetWindowLongPtr win, GWL_STYLE, windowStyle
        End If
    End If
End Sub

Function GetTitlebarSize~& ()
    Dim As POINT pt
    Dim As RECT rc
    ClientToScreen parentWin, Offset(pt)
    GetWindowRect parentWin, Offset(rc)
    GetTitlebarSize = pt.y - rc.top
End Function

Sub qPlay (fileName As String)
    If OpenMediaFile(fileName) = 0 Then
        SizeVideoWindow 1280, 720
        PlayMediaFile
    Else
        qStop
    End If
End Sub

Sub qPause ()
    Dim As String playCommand: playCommand = "Pause movie" + Chr$(0)
    mciSendString Offset(playCommand), 0, 0, 0
End Sub

Sub qResume ()
    Dim As String playCommand: playCommand = "Resume movie" + Chr$(0)
    mciSendString Offset(playCommand), 0, 0, 0
End Sub

Sub qStop ()
    ClosePlayer
    ToggleEnable pauseBtn, 0
    ToggleEnable rewindBtn, 0
    ToggleEnable ffBtn, 0
    SetWindowText text, 0
    SetTrackBarVal trackbar, 0
End Sub

Function CheckExtension%% (fileName As String)
    '*.AVI;*.MP4;*.MKV;*.MPG;*.WMV
    Dim As String extension: extension = UCase$(Right$(fileName, 4))
    Print extension
    If extension <> ".AVI" And extension <> ".MP4" And extension <> ".MKV" And extension <> ".MPG" And extension <> ".WMV" Then
        CheckExtension = 0
    Else
        CheckExtension = -1
    End If
End Function

Sub SeekLeft ()
    Dim As Unsigned Integer64 position: position = GetVideoPos
    Dim As String playCommand
    If position < 10000 Then
        playCommand = "seek movie to start notify" + Chr$(0)
    Else
        position = position - 10000
        playCommand = "seek movie to " + LTrim$(Str$(position)) + " notify" + Chr$(0)
    End If
    mciSendString Offset(playCommand), 0, 0, parentWin
    playCommand = "Play movie notify" + Chr$(0)
    mciSendString Offset(playCommand), 0, 0, parentWin
End Sub

Sub SeekRight ()
    Dim As Unsigned Integer64 position: position = GetVideoPos + 30000
    Dim As String playCommand
    If position < GetVideoLength Then
        playCommand = "seek movie to " + LTrim$(Str$(position)) + " notify" + Chr$(0)
        mciSendString Offset(playCommand), 0, 0, parentWin
        playCommand = "Play movie notify" + Chr$(0)
        mciSendString Offset(playCommand), 0, 0, parentWin
    End If
End Sub

Function GetVideoPos~&&
    Dim As String position: position = Space$(128)
    Dim As String query: query = "status movie position" + Chr$(0)
    mciSendString Offset(query), Offset(position), 128, 0
    GetVideoPos = Val(Mid$(position, 1, InStr(position, Chr$(0)) - 1))
End Function

Function OpenMediaFile~& (fileName As String)
    ClosePlayer
    If CheckExtension(fileName) = -1 Then
        Dim As Unsigned Long mciError
        Dim As String playCommand: playCommand = "Open " + Chr$(34) + fileName + Chr$(34) + " type mpegvideo alias movie" + Chr$(0)
        mciError = mciSendString(Offset(playCommand), 0, 0, 0)
        Print mciError
        If mciError <> 0 Then
            Dim As String errString: errString = Space$(128)
            mciGetErrorString mciError, Offset(errString), 128
            ErrorPopup "MCI Error Encountered", Mid$(errString, 1, InStr(errString, Chr$(0)) - 1), "Most MCI errors are caused by not having the proper codec installed. To fix this, please install the K-Lite Codec Pack Full by clicking this link:" + Chr$(10) + "<A HREF=" + Chr$(34) + "https://codecguide.com/download_k-lite_codec_pack_full.htm" + Chr$(34) + ">K-Lite Codec Pack Full Download</A>"
            isOpen = 0
        Else
            isOpen = -1
        End If
    Else
        mciError = 461
        ErrorPopup "File Format Error", "Unsupported file format", "I haven't yet checked this file type. Perhaps it is a strange one you like. However, please stick to using the ones allowed by the open file dialog." + Chr$(10) + Chr$(10) + "-Spriggsy"
    End If
    OpenMediaFile = mciError
End Function

Sub SizeVideoWindow (maxX As Unsigned Long, maxY As Unsigned Long)
    Dim As String size: size = GetVideoSize
    Dim As String rc(0 To 3)
    tokenize size, " ", rc()
    Dim As Unsigned Long videoX, videoY
    videoX = Val(rc(2))
    videoY = Val(rc(3))
    If videoX < maxX And videoY < maxY Then Exit Sub
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
    Print GetTitlebarSize
    Dim As String res: res = "put movie window at " + LTrim$(Str$(iLeft)) + " " + LTrim$(Str$(iTop)) + " " + LTrim$(Str$(newWidth)) + " " + LTrim$(Str$(newHeight + GetTitlebarSize * 4)) + Chr$(0)
    mciSendString Offset(res), 0, 0, 0
End Sub

Sub PlayMediaFile ()
    If isOpen Then
        Dim As String playCommand: playCommand = "Play movie notify" + Chr$(0)
        mciSendString Offset(playCommand), 0, 0, parentWin
    End If
End Sub

Sub ClosePlayer ()
    If isOpen Then
        Dim As String playCommand: playCommand = "Close movie notify" + Chr$(0)
        mciSendString Offset(playCommand), 0, 0, parentWin
        isOpen = 0
    End If
End Sub

Function isPlaying$ ()
    Dim As String status: status = Space$(128)
    Dim As String query: query = "status movie mode" + Chr$(0)
    mciSendString Offset(query), Offset(status), 128, 0
    isPlaying = Mid$(status, 1, InStr(status, Chr$(0)) - 1)
End Function

Sub SetTrackBarVal (tbHandle As Offset, newValue As Unsigned Integer64)
    SendMessage tbHandle, TBM_SETPOS, -61, newValue
End Sub

Sub ToggleEnable (controlHandle As Offset, __toggle As Byte)
    Select Case __toggle
        Case 1
            EnableWindow controlHandle, -1
        Case 0
            EnableWindow controlHandle, 0
    End Select
End Sub

$If PTRTOSTRING = UNDEFINED Then
    $Let PTRTOSTRING = TRUE
    Function pointerToString$ (pointer As Offset)
        Declare CustomType Library
            Function strlen%& (ByVal ptr As Unsigned Offset)
        End Declare
        Dim As Offset length: length = strlen(pointer)
        If length Then
            Dim As MEM pString: pString = Mem(pointer, length)
            Dim As String ret: ret = Space$(length)
            MemGet pString, pString.OFFSET, ret
            MemFree pString
        End If
        pointerToString = ret
    End Function
$End If

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
        StorageArray(i) = pointerToString(tokenized)
        tokenized = strtok(0, delCopy)
        i = i + 1
    Wend
    ReDim Preserve StorageArray(UBound(StorageArray) - 1)
End Sub

Function wCharPtrToString$ (wchar As _Offset)
    Declare CustomType Library
        Function wcslen%& (ByVal str As _Offset)
    End Declare
    Dim As _Offset wlen: wlen = wcslen(wchar) * 2 'The length does not account for the 2-byte nature of Unicode so we multiply by 2
    Dim As _MEM pChar: pChar = _Mem(wchar, wlen) 'Declaring a new _MEM block and setting it to grab the number of bytes referenced by wlen at pointer wchar
    Dim As String char: char = Space$(wlen) 'Declaring a new string large enough to hold the unicode string
    _MemGet pChar, pChar.OFFSET, char 'Storing the data in the string
    _MemFree pChar 'Freeing the _MEM block
    wCharPtrToString = UnicodeToANSI(char) 'Returning the converted Unicode string
End Function

'$INCLUDE:'unicodetoansi.bas'


'$Include:'OpenSave.BM'
