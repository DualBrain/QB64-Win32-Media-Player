Const COMCTL32_VERSION = 6
'Constants used to identify values used for custom buttons and the msg variable in the TaskDlgCallback function
'Consult this MSDN link: https://docs.microsoft.com/en-us/windows/win32/api/commctrl/ns-commctrl-taskdialog_button
Const TDCBF_OK_BUTTON = &H1
Const TDCBF_YES_BUTTON = &H2
Const TDCBF_NO_BUTTON = &H4
Const TDCBF_CANCEL_BUTTON = &H8
Const TDCBF_RETRY_BUTTON = &H10
Const TDCBF_CLOSE_BUTTON = &H20 'Only used for creation of a Close button. msg will return TDCBF_CANCEL_BUTTON for a Close button as they have the same functionality

'Constants used with TASKDIALOGCONFIG.nDefaultButton
Const IDCANCEL = 2
Const IDNO = 7
Const IDOK = 1
Const IDRETRY = 4
Const IDYES = 6

'Flags used with TASKDIALOGCONFIG.dwFlags. Can be any combination except those that contradict one another
'Consult this MSDN link: https://docs.microsoft.com/en-us/windows/win32/api/Commctrl/ns-commctrl-taskdialogconfig
Const TDF_ENABLE_HYPERLINKS = &H1 'Used to enable hyperlink usage, with syntax similar to html. Can be used in the body, expando, and footer. Syntax for hyperlink: <A HREF="executablestring or webpage">Hyperlink Text</A>
Const TDF_USE_HICON_MAIN = &H2 'Although the hIconMain member is not being used in my type declaration, it occupies the same space as TASKDIALOGCONFIG.pszMainIcon. An HICON returned from the ExtractIcon Win32 function can be used with this flag
Const TDF_USE_HICON_FOOTER = &H4 'Similar to the above, hIconFooter occupies the same space as TASKDIALOGCONFIG.pszIconFooter. An HICON returned from the ExtractIcon Win32 function can be used with this flag
Const TDF_ALLOW_DIALOG_CANCELLATION = &H8 'Allows dialog to be closed using Alt+F4, Escape, and the title bar's close button even if no cancel/close button is defined in TASKDIALOG.dwCommonButtons or TASKDIALOG.pButtons
Const TDF_USE_COMMAND_LINKS = &H10 'uses command links rather than normal buttons. All text before a newline character (Chr$(10)) will be the main text. All text after a newline character will be a footnote
Const TDF_USE_COMMAND_LINKS_NO_ICON = &H20 'same as above, but no icons on the buttons. Do not use in conjunction with TD_USE_COMMAND_LINKS. All text before a newline character (Chr$(10)) will be the main text. All text after a newline character will be a footnote
Const TDF_EXPAND_FOOTER_AREA = &H40 'TASKDIALOGCONFIG.pszExpandedInformation will be displayed at the bottom of the footer instead of immediately after the content element
Const TDF_EXPANDED_BY_DEFAULT = &H80 'Expando element will be already expanded on creation of the dialog by default
Const TDF_VERIFICATION_FLAG_CHECKED = &H100 'Checkbox element will be already expanded on creation of the dialog by default
Const TDF_SHOW_PROGRESS_BAR = &H0200 'A progress bar element will be created and displayed in the dialog on creation
Const TDF_SHOW_MARQUEE_PROGRESS_BAR = &H0400 'A progress bar element will be created and use the marquee style on creation
Const TDF_CALLBACK_TIMER = &H0800 'A timer will be initialized for the TaskDlgCallback function. wParam in the TaskDlgCallback function will contain the time elapsed since creation when handling the TDN_TIMER message
Const TDF_POSITION_RELATIVE_TO_WINDOW = &H1000 'Dialog box will be centered relative to the parent window specified in TASKDIALOG.hwndParent
Const TDF_RTL_LAYOUT = &H2000 'Text will be displayed reading right to left
Const TDF_NO_DEFAULT_RADIO_BUTTON = &H4000 'No radio button will be selected by default
Const TDF_CAN_BE_MINIMIZED = &H8000 'Allows dialog to be minimized
Const TDF_SIZE_TO_CONTENT = &H1000000 'Dialog will be autosized to fit the body (content) area. Ignored if TASKDIALOGCONFIG.cxWidth is not 0

'Messages returned in the msg variable of the TaskDlgCallback function
'Consult this MSDN link: https://docs.microsoft.com/en-us/windows/win32/controls/bumper-task-dialogs-reference-notifications
Const TDN_BUTTON_CLICKED = 2 'Button has been clicked. wParam is nonzero if checked, zero if unchecked.
Const TDN_CREATED = 0 'Dialog is displayed
Const TDN_DESTROYED = 5 'Dialog has been destroyed either by user or program error
Const TDN_DIALOG_CONSTRUCTED = 7 'Dialog has been created and ready to be displayed
Const TDN_EXPANDO_BUTTON_CLICKED = 10 'Expando has been clicked. wParam is nonzero if checked, zero if unchecked.
Const TDN_HELP = 9 'F1 key has been pressed
Const TDN_HYPERLINK_CLICKED = 3 'Hyperlink text has been clicked. lParam contains a pointer to the wide char string for the hyperlink clicked.
Const TDN_NAVIGATED = 1 'Dialog has changed pages/forms
Const TDN_RADIO_BUTTON_CLICKED = 6 'Radio button has been clicked. wParam in TaskDlgCallback contains the ID of the radio button
Const TDN_TIMER = 4 'Is sent every 200 ms. wParam contains the total elapsed time since dialog creation
Const TDN_VERIFICATION_CLICKED = 8 'Checkbox clicked. wParam is nonzero if checked, zero if unchecked.

'Flags used with SendMessage to update the dialog box after creation
'Consult this MSDN link: https://docs.microsoft.com/en-us/windows/win32/controls/bumper-task-dialogs-reference-messages
Const WM_USER = &H0400
Const TDM_CLICK_BUTTON = WM_USER + 102 'Simulating clicking a button on the dialog. Example: SendMessage(hwnd, TDM_CLICK_BUTTON,TDCBF_NO_BUTTON, 0)
Const TDM_CLICK_RADIO_BUTTON = WM_USER + 110 'Simulating clicking a radio button on the dialog. Example: SendMessage(hwnd, TDM_CLICK_RADIO_BUTTON, 1, 0)
Const TDM_CLICK_VERIFICATION = WM_USER + 113 'Simulating clicking the checkbox on the dialog. 1 for checked, 0 for unchecked. Example: SendMessage(hwnd, TDM_CLICK_VERIFICATION, 1, 0)
Const TDM_ENABLE_BUTTON = WM_USER + 111 'Enables or disables a button on the dialog. Nonzero for enabled, 0 for disabled. Example: SendMessage(hwnd, TDM_ENABLE_BUTTON, TDCBF_YES_BUTTON, 1)
Const TDM_ENABLE_RADIO_BUTTON = WM_USER + 112 'Enables or disables a radio button on the dialog. Nonzero for enabled, 0 for disabled. Example: SendMessage(hwnd, TDM_ENABLE_RADIO_BUTTON, 2, 0)
Const TDM_NAVIGATE_PAGE = WM_USER + 101 'Causes dialog to change to new form/page. You create a new TASKDIALOGCONFIG variable and pass it with _Offset in the lParam argument of SendMessage. Example: SendMessage(hwnd, TDM_NAVIGATE_PAGE, 0, _Offset(newTaskDialogPage))
Const TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE = WM_USER + 115 'Places shield icon on button or command link to notify that the action will require elevation. Example: SendMessage(hwnd, TDM_SET_BUTTON_ELEVATION_REQUIRED_STATE, TDCBF_YES_BUTTON, 1)
Const TDM_SET_ELEMENT_TEXT = WM_USER + 108 'Can update text in content, expando info, footer, and header. Example: SendMessage(hwnd, TDM_SET_ELEMENT_TEXT, TDE_CONTENT, _Offset(newTextString)) 'This will also require that the string be null-terminated and converted to Unicode
Const TDM_SET_MARQUEE_PROGRESS_BAR = WM_USER + 103 'Sets the status of marquee mode for progress bar. 1 for enabled, 0 for disabled. Example: SendMessage(hwnd, TDM_SET_MARQUEE_PROGRESS_BAR, 1, 0)
Const TDM_SET_PROGRESS_BAR_MARQUEE = WM_USER + 107 'Turns marquee display on or off and sets animation speed. Example: SendMessage(hwnd, TDM_SET_PROGRESS_BAR_MARQUEE, 1, 20)
Const TDM_SET_PROGRESS_BAR_POS = WM_USER + 106 'Updates the progress bar to a new value to indicate progress. Example: SendMessage(hwnd, TDM_SET_PROGRESS_BAR_POS, 50, 0)
Const TDM_SET_PROGRESS_BAR_RANGE = WM_USER + 105 'Sets range of progress bar. Use MAKELPARAM to make a new range. Example: SendMessage(hwnd, TDM_SET_PROGRESS_BAR_RANGE, 0, MAKELONG(10, 120))
Const TDM_SET_PROGRESS_BAR_STATE = WM_USER + 104 'Sets state of progress bar. Example: SendMessage(hwnd, TDM_SET_PROGRESS_BAR_STATE, PBST_PAUSED, 0)
Const TDM_UPDATE_ELEMENT_TEXT = WM_USER + 114 'Same result as TDM_SET_ELEMENT_TEXT
Const TDM_UPDATE_ICON = WM_USER + 116 'Updates the main or footer icon. Example: SendMessage(hwnd, TDM_UPDATE_ICON, TDIE_ICON_MAIN, TD_SHIELD_ICON)

'Flags used with SendMessage and TDM_UPDATE_ELEMENT_TEXT or TDM_SET_ELEMENT_TEXT to specify which element to update
Const TDE_CONTENT = 0
Const TDE_EXPANDED_INFORMATION = 1
Const TDE_FOOTER = 2
Const TDE_MAIN_INSTRUCTION = 3

'Flags used with SendMessage and TDM_UPDATE_ICON to specify which icon to update
Const TDIE_ICON_MAIN = 0
Const TDIE_ICON_FOOTER = 1

'Flags used with SendMessage and TDM_SET_PROGRESS_BAR_STATE to change state of the progress bar
Const PBST_NORMAL = 1
Const PBST_PAUSED = 3
Const PBST_ERROR = 2

'Type used in an array to define each individual custom button displayed
'Consult this MSDN link: https://docs.microsoft.com/en-us/windows/win32/api/commctrl/ns-commctrl-taskdialog_button
Type TASKDIALOG_BUTTON
    As Long nButtonID 'Can be any TDCBF button constant
    As _Offset pszButtonText 'The custom text to be displayed in the button. Must be null-terminated (Chr$(0)) and use ANSIToUnicode conversion
End Type

'Type used to specify configuration of a new task dialog.
'Consult this MSDN link: https://docs.microsoft.com/en-us/windows/win32/api/Commctrl/ns-commctrl-taskdialogconfig
Type TASKDIALOGCONFIG
    As _Unsigned Long cbSize 'Mandatory and must be set so the API knows how large the TYPE is. Set this element using Len and passing the name of the TASKDIALOGCONFIG structure
    As _Offset hwndParent, hInstance 'hInstance is optional and most always will be zero. hwndParent is optional and can be _WindowHandle or zero.
    As Long dwFlags, dwCommonButtons 'dwFlags can be any combination of TDF flags, with exceptions of some that contradict one another. dwCommonButtons can be any TDCBF button constant
    As _Offset pszWindowTitle, pszMainIcon, pszMainInstruction, pszContent 'All four are pointers to strings with the exception of pszMainIcon. pszMainIcon references (currently) any icon listed in tdicon.h
    As _Unsigned Long cButtons 'Refers to the number of buttons in the TASKDIALOG_BUTTON structure array
    As _Offset pButtons 'Pointer to an array of TASKDIALOG_BUTTON structures
    As Long nDefaultButton 'The default button that will be highlighted in the dialog. Can be any ID constant
    As _Unsigned Long cRadioButtons 'Refers to the number of buttons in the TASKDIALOG_BUTTON structure array
    As _Offset pRadioButtons 'Pointer to an array of TASKDIALOG_BUTTON structures
    As Long nDefaultRadioButton 'ID for default radio button
    'The following are all pointers to null-terminated, Unicode strings. pszVerificationText sets checkbox text. pszExpandedInformation sets expando info.
    'pszExpandedControlText sets text contained in expando. pszCollapsedControlText sets text of collapsed expando.
    'pszFooter icon refers to any icon in tdicon.h. pszFooter is a null-terminated, Unicode string to set footer text. pfCallback is set to the TaskDialogCallback function pointer which calls our TaskDlgCallback function
    'lpCallbackdata is a pointer to any data needed to be passed to the dialog's callback function. I recommend using a _MEM block so you can store anything in any amount and then unpack it in the callback function
    As _Offset pszVerificationText, pszExpandedInformation, pszExpandedControlText, pszCollapsedControlText, pszFooterIcon, pszFooter, pfCallback, lpCallbackData
    As _Unsigned Long cxWidth 'Sets width of dialog. If 0, task dialog will calculate ideal width
End Type

'Consult this MSDN link: https://docs.microsoft.com/en-us/windows/win32/api/commctrl/nf-commctrl-taskdialogindirect
Declare Dynamic Library "Comctl32"
    Function TaskDialogIndirect%& (ByVal pTaskConfig As _Offset, Byval pnButton As _Offset, Byval pnRadioButton As _Offset, Byval pfVerificationFlagChecked As _Offset)
    Sub TaskDialogIndirect (ByVal pTaskConfig As _Offset, Byval pnButton As _Offset, Byval pnRadioButton As _Offset, Byval pfVerificationFlagChecked As _Offset)
End Declare

'Tests for success. Consult this MSDN link: https://docs.microsoft.com/en-us/windows/win32/api/winerror/nf-winerror-succeeded
Declare Library "internal\c\c_compiler\x86_64-w64-mingw32\include\winerror"
    Function SUCCEEDED%% (ByVal hr As _Offset)
End Declare

Declare CustomType Library "Resources\Headers\tdicon" 'This library contains all the common icons I could find to use with this setup
    Function TD_ERROR_ICON%& Alias "task_error_icon"
    Function TD_WARNING_ICON%& Alias "task_warning_icon"
    Function TD_INFORMATION_ICON%& Alias "task_info_icon"
    Function TD_SHIELD_ICON%& Alias "task_shield_icon"
    Function TD_SEC_SHIELD_ICON%& Alias "task_sec_shield" 'TD_SEC icons have banners
    Function TD_SEC_WARN_ICON%& Alias "task_sec_warning"
    Function TD_SEC_ERROR_ICON%& Alias "task_sec_error"
    Function TD_SEC_SUCCESS_ICON%& Alias "task_sec_success"
End Declare

'This is the function that returns a pointer to the callback function.
'Consult this MSDN link:https://docs.microsoft.com/en-us/windows/win32/api/commctrl/nc-commctrl-pftaskdialogcallback
Declare CustomType Library "Resources\Headers\taskdlg"
    Function TaskDialogCallback%& Alias "TaskDialogCallbackProc"
End Declare

Declare CustomType Library "Resources\Headers\makeint"
    Function MAKEINTRESOURCE%& Alias "MAKEINTRSC" (ByVal i As _Offset)
End Declare
