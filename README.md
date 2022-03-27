# QB64-Win32-Video-Player
Playing videos in QB64 using Win32 and MCI

This code will allow you to play any video in QB64 using Windows API. Several APIs are being used in this project (Winmm, Comctl, User32, Shell32.... to name a few). This is still a work in progress. I am aware that the program will crash if an unsupported video is used. However, if you install the K-Lite package https://codecguide.com/download_k-lite_codec_pack_full.htm then you should not experience this issue. I'll eventually add checking for unsupported videos but right now this is a good starting point for those who might want to study the code.

![Using the trackbar to change video position](https://github.com/SpriggsySpriggs/QB64-Win32-Video-Player/blob/master/Resources/ezgif-2-dc76b25357.gif)
