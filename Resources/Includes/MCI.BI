Const MM_MCINOTIFY = &H3B9
Const MCI_NOTIFY_SUCCESSFUL = &H0001
Const MCI_NOTIFY_SUPERSEDED = &H0002
Const MCI_NOTIFY_ABORTED = &H0004
Const MCI_NOTIFY_FAILURE = &H0008

Declare Dynamic Library "Winmm"
    Function mciSendStringA~& (lpszCommand As String, Byval lpszReturnString As _Offset, Byval cchReturn As _Unsigned Long, Byval hwndCallback As _Offset)
    Sub mciSendStringA (lpszCommand As String, Byval lpszReturnString As _Offset, Byval cchReturn As _Unsigned Long, Byval hwndCallback As _Offset)
    Sub mciGetErrorStringA (ByVal fdwError As _Unsigned Long, Byval lpszErrorText As _Offset, Byval cchErrorText As _Unsigned Long)

    Function mciSendStringW~& (lpszCommand As String, Byval lpszReturnString As _Offset, Byval cchReturn As _Unsigned Long, Byval hwndCallback As _Offset)
    Sub mciSendStringW (lpszCommand As String, Byval lpszReturnString As _Offset, Byval cchReturn As _Unsigned Long, Byval hwndCallback As _Offset)
    Sub mciGetErrorStringW (ByVal fdwError As _Unsigned Long, Byval lpszErrorText As _Offset, Byval cchErrorText As _Unsigned Long)
End Declare
