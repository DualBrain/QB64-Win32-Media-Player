Declare CustomType Library
    Function WideCharToMultiByte& (ByVal CodePage As _Unsigned Long, Byval dwFlags As Long, Byval lpWideCharStr As _Offset, Byval cchWideChar As Integer, Byval lpMultiByteStr As _Offset, Byval cbMultiByte As Integer, Byval lpDefaultChar As _Offset, Byval lpUsedDefaultChar As _Offset)
    Function MultiByteToWideChar& (ByVal CodePage As _Unsigned Long, Byval dwFlags As Long, Byval lpMultiByteStr As _Offset, Byval cbMultiByte As Integer, Byval lpWideCharStr As _Offset, Byval cchWideChar As Integer)
End Declare

Function UnicodeToANSI$ (buffer As String)
    Dim As String ansibuffer: ansibuffer = Space$(Len(buffer))
    Dim As Long a: a = WideCharToMultiByte(437, 0, _Offset(buffer), Len(buffer), _Offset(ansibuffer), Len(ansibuffer), 0, 0)
    UnicodeToANSI = Mid$(ansibuffer, 1, InStr(ansibuffer, Chr$(0)) - 1)
End Function

Sub UnicodeToANSI (buffer As String, __dest As String)
    Dim As String ansibuffer: ansibuffer = Space$(Len(buffer))
    Dim As Long a: a = WideCharToMultiByte(437, 0, _Offset(buffer), Len(buffer), _Offset(ansibuffer), Len(ansibuffer), 0, 0)
    __dest = Mid$(ansibuffer, 1, InStr(ansibuffer, Chr$(0)) - 1)
End Sub

Function ANSIToUnicode$ (buffer As String)
    Dim As String unicodebuffer: unicodebuffer = Space$(Len(buffer) * 2)
    Dim As Long a: a = MultiByteToWideChar(65001, 0, _Offset(buffer), Len(buffer), _Offset(unicodebuffer), Len(unicodebuffer))
    ANSIToUnicode = unicodebuffer
End Function

Sub ANSIToUnicode (buffer As String, __dest As String)
    Dim As String unicodebuffer: unicodebuffer = Space$(Len(buffer) * 2)
    Dim As Long a: a = MultiByteToWideChar(65001, 0, _Offset(buffer), Len(buffer), _Offset(unicodebuffer), Len(unicodebuffer))
    __dest = unicodebuffer
End Sub
