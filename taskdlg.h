ptrszint FUNC_TASKDLGCALLBACK(ptrszint*_FUNC_TASKDLGCALLBACK_OFFSET_HWND,uint32*_FUNC_TASKDLGCALLBACK_ULONG_MSG,uptrszint*_FUNC_TASKDLGCALLBACK_UOFFSET_WPARAM,ptrszint*_FUNC_TASKDLGCALLBACK_OFFSET_LPARAM,ptrszint*_FUNC_TASKDLGCALLBACK_OFFSET_LPREFDATA);

HRESULT TaskDialogCallback(HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam, LONG_PTR lpRefData){
	return FUNC_TASKDLGCALLBACK((ptrszint *) (& hwnd), & Msg, & wParam, (ptrszint *) (& lParam), &lpRefData);
}

void * TaskDialogCallbackProc(){
	return (void *) TaskDialogCallback;
}