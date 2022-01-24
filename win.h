ptrszint FUNC_WINDOWPROC(ptrszint*_FUNC_WINDOWPROC_OFFSET_HWND,uint32*_FUNC_WINDOWPROC_ULONG_UMSG,uptrszint*_FUNC_WINDOWPROC_UOFFSET_WPARAM,ptrszint*_FUNC_WINDOWPROC_OFFSET_LPARAM);

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
 return FUNC_WINDOWPROC((ptrszint *) (& hwnd), & uMsg, & wParam, (ptrszint *) (& lParam));
}

void * GetWindowProc() {
 return (void *) WindowProc;
}