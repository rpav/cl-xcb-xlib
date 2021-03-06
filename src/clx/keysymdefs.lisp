(in-package :xcb.clx)

(define-keysym :|VoidSymbol| #xFFFFFF)
(define-keysym :|BackSpace| #xFF08)
(define-keysym :|Tab| #xFF09)
(define-keysym :|Linefeed| #xFF0A)
(define-keysym :|Clear| #xFF0B)
(define-keysym :|Return| #xFF0D)
(define-keysym :|Pause| #xFF13)
(define-keysym :|Scroll_Lock| #xFF14)
(define-keysym :|Sys_Req| #xFF15)
(define-keysym :|Escape| #xFF1B)
(define-keysym :|Delete| #xFFFF)
(define-keysym :|Multi_key| #xFF20)
(define-keysym :|Codeinput| #xFF37)
(define-keysym :|SingleCandidate| #xFF3C)
(define-keysym :|MultipleCandidate| #xFF3D)
(define-keysym :|PreviousCandidate| #xFF3E)
(define-keysym :|Kanji| #xFF21)
(define-keysym :|Muhenkan| #xFF22)
(define-keysym :|Henkan_Mode| #xFF23)
(define-keysym :|Henkan| #xFF23)
(define-keysym :|Romaji| #xFF24)
(define-keysym :|Hiragana| #xFF25)
(define-keysym :|Katakana| #xFF26)
(define-keysym :|Hiragana_Katakana| #xFF27)
(define-keysym :|Zenkaku| #xFF28)
(define-keysym :|Hankaku| #xFF29)
(define-keysym :|Zenkaku_Hankaku| #xFF2A)
(define-keysym :|Touroku| #xFF2B)
(define-keysym :|Massyo| #xFF2C)
(define-keysym :|Kana_Lock| #xFF2D)
(define-keysym :|Kana_Shift| #xFF2E)
(define-keysym :|Eisu_Shift| #xFF2F)
(define-keysym :|Eisu_toggle| #xFF30)
(define-keysym :|Kanji_Bangou| #xFF37)
(define-keysym :|Zen_Koho| #xFF3D)
(define-keysym :|Mae_Koho| #xFF3E)
(define-keysym :|Home| #xFF50)
(define-keysym :|Left| #xFF51)
(define-keysym :|Up| #xFF52)
(define-keysym :|Right| #xFF53)
(define-keysym :|Down| #xFF54)
(define-keysym :|Prior| #xFF55)
(define-keysym :|Page_Up| #xFF55)
(define-keysym :|Next| #xFF56)
(define-keysym :|Page_Down| #xFF56)
(define-keysym :|End| #xFF57)
(define-keysym :|Begin| #xFF58)
(define-keysym :|Select| #xFF60)
(define-keysym :|Print| #xFF61)
(define-keysym :|Execute| #xFF62)
(define-keysym :|Insert| #xFF63)
(define-keysym :|Undo| #xFF65)
(define-keysym :|Redo| #xFF66)
(define-keysym :|Menu| #xFF67)
(define-keysym :|Find| #xFF68)
(define-keysym :|Cancel| #xFF69)
(define-keysym :|Help| #xFF6A)
(define-keysym :|Break| #xFF6B)
(define-keysym :|Mode_switch| #xFF7E)
(define-keysym :|script_switch| #xFF7E)
(define-keysym :|Num_Lock| #xFF7F)
(define-keysym :|KP_Space| #xFF80)
(define-keysym :|KP_Tab| #xFF89)
(define-keysym :|KP_Enter| #xFF8D)
(define-keysym :KP_F1 #xFF91)
(define-keysym :KP_F2 #xFF92)
(define-keysym :KP_F3 #xFF93)
(define-keysym :KP_F4 #xFF94)
(define-keysym :|KP_Home| #xFF95)
(define-keysym :|KP_Left| #xFF96)
(define-keysym :|KP_Up| #xFF97)
(define-keysym :|KP_Right| #xFF98)
(define-keysym :|KP_Down| #xFF99)
(define-keysym :|KP_Prior| #xFF9A)
(define-keysym :|KP_Page_Up| #xFF9A)
(define-keysym :|KP_Next| #xFF9B)
(define-keysym :|KP_Page_Down| #xFF9B)
(define-keysym :|KP_End| #xFF9C)
(define-keysym :|KP_Begin| #xFF9D)
(define-keysym :|KP_Insert| #xFF9E)
(define-keysym :|KP_Delete| #xFF9F)
(define-keysym :|KP_Equal| #xFFBD)
(define-keysym :|KP_Multiply| #xFFAA)
(define-keysym :|KP_Add| #xFFAB)
(define-keysym :|KP_Separator| #xFFAC)
(define-keysym :|KP_Subtract| #xFFAD)
(define-keysym :|KP_Decimal| #xFFAE)
(define-keysym :|KP_Divide| #xFFAF)
(define-keysym :KP_0 #xFFB0)
(define-keysym :KP_1 #xFFB1)
(define-keysym :KP_2 #xFFB2)
(define-keysym :KP_3 #xFFB3)
(define-keysym :KP_4 #xFFB4)
(define-keysym :KP_5 #xFFB5)
(define-keysym :KP_6 #xFFB6)
(define-keysym :KP_7 #xFFB7)
(define-keysym :KP_8 #xFFB8)
(define-keysym :KP_9 #xFFB9)
(define-keysym :F1 #xFFBE)
(define-keysym :F2 #xFFBF)
(define-keysym :F3 #xFFC0)
(define-keysym :F4 #xFFC1)
(define-keysym :F5 #xFFC2)
(define-keysym :F6 #xFFC3)
(define-keysym :F7 #xFFC4)
(define-keysym :F8 #xFFC5)
(define-keysym :F9 #xFFC6)
(define-keysym :F10 #xFFC7)
(define-keysym :F11 #xFFC8)
(define-keysym :L1 #xFFC8)
(define-keysym :F12 #xFFC9)
(define-keysym :L2 #xFFC9)
(define-keysym :F13 #xFFCA)
(define-keysym :L3 #xFFCA)
(define-keysym :F14 #xFFCB)
(define-keysym :L4 #xFFCB)
(define-keysym :F15 #xFFCC)
(define-keysym :L5 #xFFCC)
(define-keysym :F16 #xFFCD)
(define-keysym :L6 #xFFCD)
(define-keysym :F17 #xFFCE)
(define-keysym :L7 #xFFCE)
(define-keysym :F18 #xFFCF)
(define-keysym :L8 #xFFCF)
(define-keysym :F19 #xFFD0)
(define-keysym :L9 #xFFD0)
(define-keysym :F20 #xFFD1)
(define-keysym :L10 #xFFD1)
(define-keysym :F21 #xFFD2)
(define-keysym :R1 #xFFD2)
(define-keysym :F22 #xFFD3)
(define-keysym :R2 #xFFD3)
(define-keysym :F23 #xFFD4)
(define-keysym :R3 #xFFD4)
(define-keysym :F24 #xFFD5)
(define-keysym :R4 #xFFD5)
(define-keysym :F25 #xFFD6)
(define-keysym :R5 #xFFD6)
(define-keysym :F26 #xFFD7)
(define-keysym :R6 #xFFD7)
(define-keysym :F27 #xFFD8)
(define-keysym :R7 #xFFD8)
(define-keysym :F28 #xFFD9)
(define-keysym :R8 #xFFD9)
(define-keysym :F29 #xFFDA)
(define-keysym :R9 #xFFDA)
(define-keysym :F30 #xFFDB)
(define-keysym :R10 #xFFDB)
(define-keysym :F31 #xFFDC)
(define-keysym :R11 #xFFDC)
(define-keysym :F32 #xFFDD)
(define-keysym :R12 #xFFDD)
(define-keysym :F33 #xFFDE)
(define-keysym :R13 #xFFDE)
(define-keysym :F34 #xFFDF)
(define-keysym :R14 #xFFDF)
(define-keysym :F35 #xFFE0)
(define-keysym :R15 #xFFE0)
(define-keysym :|Shift_L| #xFFE1)
(define-keysym :|Shift_R| #xFFE2)
(define-keysym :|Control_L| #xFFE3)
(define-keysym :|Control_R| #xFFE4)
(define-keysym :|Caps_Lock| #xFFE5)
(define-keysym :|Shift_Lock| #xFFE6)
(define-keysym :|Meta_L| #xFFE7)
(define-keysym :|Meta_R| #xFFE8)
(define-keysym :|Alt_L| #xFFE9)
(define-keysym :|Alt_R| #xFFEA)
(define-keysym :|Super_L| #xFFEB)
(define-keysym :|Super_R| #xFFEC)
(define-keysym :|Hyper_L| #xFFED)
(define-keysym :|Hyper_R| #xFFEE)
(define-keysym :|ISO_Group_Shift| #xFF7E)
(define-keysym #\LATIN_CAPITAL_LETTER_A_WITH_OGONEK #x1A1)
(define-keysym #\BREVE #x1A2)
(define-keysym #\LATIN_CAPITAL_LETTER_L_WITH_STROKE #x1A3)
(define-keysym #\LATIN_CAPITAL_LETTER_L_WITH_CARON #x1A5)
(define-keysym #\LATIN_CAPITAL_LETTER_S_WITH_ACUTE #x1A6)
(define-keysym #\LATIN_CAPITAL_LETTER_S_WITH_CARON #x1A9)
(define-keysym #\LATIN_CAPITAL_LETTER_S_WITH_CEDILLA #x1AA)
(define-keysym #\LATIN_CAPITAL_LETTER_T_WITH_CARON #x1AB)
(define-keysym #\LATIN_CAPITAL_LETTER_Z_WITH_ACUTE #x1AC)
(define-keysym #\LATIN_CAPITAL_LETTER_Z_WITH_CARON #x1AE)
(define-keysym #\LATIN_CAPITAL_LETTER_Z_WITH_DOT_ABOVE #x1AF)
(define-keysym #\LATIN_SMALL_LETTER_A_WITH_OGONEK #x1B1)
(define-keysym #\OGONEK #x1B2)
(define-keysym #\LATIN_SMALL_LETTER_L_WITH_STROKE #x1B3)
(define-keysym #\LATIN_SMALL_LETTER_L_WITH_CARON #x1B5)
(define-keysym #\LATIN_SMALL_LETTER_S_WITH_ACUTE #x1B6)
(define-keysym #\CARON #x1B7)
(define-keysym #\LATIN_SMALL_LETTER_S_WITH_CARON #x1B9)
(define-keysym #\LATIN_SMALL_LETTER_S_WITH_CEDILLA #x1BA)
(define-keysym #\LATIN_SMALL_LETTER_T_WITH_CARON #x1BB)
(define-keysym #\LATIN_SMALL_LETTER_Z_WITH_ACUTE #x1BC)
(define-keysym #\DOUBLE_ACUTE_ACCENT #x1BD)
(define-keysym #\LATIN_SMALL_LETTER_Z_WITH_CARON #x1BE)
(define-keysym #\LATIN_SMALL_LETTER_Z_WITH_DOT_ABOVE #x1BF)
(define-keysym #\LATIN_CAPITAL_LETTER_R_WITH_ACUTE #x1C0)
(define-keysym #\LATIN_CAPITAL_LETTER_A_WITH_BREVE #x1C3)
(define-keysym #\LATIN_CAPITAL_LETTER_L_WITH_ACUTE #x1C5)
(define-keysym #\LATIN_CAPITAL_LETTER_C_WITH_ACUTE #x1C6)
(define-keysym #\LATIN_CAPITAL_LETTER_C_WITH_CARON #x1C8)
(define-keysym #\LATIN_CAPITAL_LETTER_E_WITH_OGONEK #x1CA)
(define-keysym #\LATIN_CAPITAL_LETTER_E_WITH_CARON #x1CC)
(define-keysym #\LATIN_CAPITAL_LETTER_D_WITH_CARON #x1CF)
(define-keysym #\LATIN_CAPITAL_LETTER_D_WITH_STROKE #x1D0)
(define-keysym #\LATIN_CAPITAL_LETTER_N_WITH_ACUTE #x1D1)
(define-keysym #\LATIN_CAPITAL_LETTER_N_WITH_CARON #x1D2)
(define-keysym #\LATIN_CAPITAL_LETTER_O_WITH_DOUBLE_ACUTE #x1D5)
(define-keysym #\LATIN_CAPITAL_LETTER_R_WITH_CARON #x1D8)
(define-keysym #\LATIN_CAPITAL_LETTER_U_WITH_RING_ABOVE #x1D9)
(define-keysym #\LATIN_CAPITAL_LETTER_U_WITH_DOUBLE_ACUTE #x1DB)
(define-keysym #\LATIN_CAPITAL_LETTER_T_WITH_CEDILLA #x1DE)
(define-keysym #\LATIN_SMALL_LETTER_R_WITH_ACUTE #x1E0)
(define-keysym #\LATIN_SMALL_LETTER_A_WITH_BREVE #x1E3)
(define-keysym #\LATIN_SMALL_LETTER_L_WITH_ACUTE #x1E5)
(define-keysym #\LATIN_SMALL_LETTER_C_WITH_ACUTE #x1E6)
(define-keysym #\LATIN_SMALL_LETTER_C_WITH_CARON #x1E8)
(define-keysym #\LATIN_SMALL_LETTER_E_WITH_OGONEK #x1EA)
(define-keysym #\LATIN_SMALL_LETTER_E_WITH_CARON #x1EC)
(define-keysym #\LATIN_SMALL_LETTER_D_WITH_CARON #x1EF)
(define-keysym #\LATIN_SMALL_LETTER_D_WITH_STROKE #x1F0)
(define-keysym #\LATIN_SMALL_LETTER_N_WITH_ACUTE #x1F1)
(define-keysym #\LATIN_SMALL_LETTER_N_WITH_CARON #x1F2)
(define-keysym #\LATIN_SMALL_LETTER_O_WITH_DOUBLE_ACUTE #x1F5)
(define-keysym #\LATIN_SMALL_LETTER_R_WITH_CARON #x1F8)
(define-keysym #\LATIN_SMALL_LETTER_U_WITH_RING_ABOVE #x1F9)
(define-keysym #\LATIN_SMALL_LETTER_U_WITH_DOUBLE_ACUTE #x1FB)
(define-keysym #\LATIN_SMALL_LETTER_T_WITH_CEDILLA #x1FE)
(define-keysym #\DOT_ABOVE #x1FF)
(define-keysym #\LATIN_CAPITAL_LETTER_H_WITH_STROKE #x2A1)
(define-keysym #\LATIN_CAPITAL_LETTER_H_WITH_CIRCUMFLEX #x2A6)
(define-keysym #\LATIN_CAPITAL_LETTER_I_WITH_DOT_ABOVE #x2A9)
(define-keysym #\LATIN_CAPITAL_LETTER_G_WITH_BREVE #x2AB)
(define-keysym #\LATIN_CAPITAL_LETTER_J_WITH_CIRCUMFLEX #x2AC)
(define-keysym #\LATIN_SMALL_LETTER_H_WITH_STROKE #x2B1)
(define-keysym #\LATIN_SMALL_LETTER_H_WITH_CIRCUMFLEX #x2B6)
(define-keysym #\LATIN_SMALL_LETTER_DOTLESS_I #x2B9)
(define-keysym #\LATIN_SMALL_LETTER_G_WITH_BREVE #x2BB)
(define-keysym #\LATIN_SMALL_LETTER_J_WITH_CIRCUMFLEX #x2BC)
(define-keysym #\LATIN_CAPITAL_LETTER_C_WITH_DOT_ABOVE #x2C5)
(define-keysym #\LATIN_CAPITAL_LETTER_C_WITH_CIRCUMFLEX #x2C6)
(define-keysym #\LATIN_CAPITAL_LETTER_G_WITH_DOT_ABOVE #x2D5)
(define-keysym #\LATIN_CAPITAL_LETTER_G_WITH_CIRCUMFLEX #x2D8)
(define-keysym #\LATIN_CAPITAL_LETTER_U_WITH_BREVE #x2DD)
(define-keysym #\LATIN_CAPITAL_LETTER_S_WITH_CIRCUMFLEX #x2DE)
(define-keysym #\LATIN_SMALL_LETTER_C_WITH_DOT_ABOVE #x2E5)
(define-keysym #\LATIN_SMALL_LETTER_C_WITH_CIRCUMFLEX #x2E6)
(define-keysym #\LATIN_SMALL_LETTER_G_WITH_DOT_ABOVE #x2F5)
(define-keysym #\LATIN_SMALL_LETTER_G_WITH_CIRCUMFLEX #x2F8)
(define-keysym #\LATIN_SMALL_LETTER_U_WITH_BREVE #x2FD)
(define-keysym #\LATIN_SMALL_LETTER_S_WITH_CIRCUMFLEX #x2FE)
(define-keysym #\LATIN_SMALL_LETTER_KRA #x3A2)
(define-keysym #\LATIN_CAPITAL_LETTER_R_WITH_CEDILLA #x3A3)
(define-keysym #\LATIN_CAPITAL_LETTER_I_WITH_TILDE #x3A5)
(define-keysym #\LATIN_CAPITAL_LETTER_L_WITH_CEDILLA #x3A6)
(define-keysym #\LATIN_CAPITAL_LETTER_E_WITH_MACRON #x3AA)
(define-keysym #\LATIN_CAPITAL_LETTER_G_WITH_CEDILLA #x3AB)
(define-keysym #\LATIN_CAPITAL_LETTER_T_WITH_STROKE #x3AC)
(define-keysym #\LATIN_SMALL_LETTER_R_WITH_CEDILLA #x3B3)
(define-keysym #\LATIN_SMALL_LETTER_I_WITH_TILDE #x3B5)
(define-keysym #\LATIN_SMALL_LETTER_L_WITH_CEDILLA #x3B6)
(define-keysym #\LATIN_SMALL_LETTER_E_WITH_MACRON #x3BA)
(define-keysym #\LATIN_SMALL_LETTER_G_WITH_CEDILLA #x3BB)
(define-keysym #\LATIN_SMALL_LETTER_T_WITH_STROKE #x3BC)
(define-keysym #\LATIN_CAPITAL_LETTER_ENG #x3BD)
(define-keysym #\LATIN_SMALL_LETTER_ENG #x3BF)
(define-keysym #\LATIN_CAPITAL_LETTER_A_WITH_MACRON #x3C0)
(define-keysym #\LATIN_CAPITAL_LETTER_I_WITH_OGONEK #x3C7)
(define-keysym #\LATIN_CAPITAL_LETTER_E_WITH_DOT_ABOVE #x3CC)
(define-keysym #\LATIN_CAPITAL_LETTER_I_WITH_MACRON #x3CF)
(define-keysym #\LATIN_CAPITAL_LETTER_N_WITH_CEDILLA #x3D1)
(define-keysym #\LATIN_CAPITAL_LETTER_O_WITH_MACRON #x3D2)
(define-keysym #\LATIN_CAPITAL_LETTER_K_WITH_CEDILLA #x3D3)
(define-keysym #\LATIN_CAPITAL_LETTER_U_WITH_OGONEK #x3D9)
(define-keysym #\LATIN_CAPITAL_LETTER_U_WITH_TILDE #x3DD)
(define-keysym #\LATIN_CAPITAL_LETTER_U_WITH_MACRON #x3DE)
(define-keysym #\LATIN_SMALL_LETTER_A_WITH_MACRON #x3E0)
(define-keysym #\LATIN_SMALL_LETTER_I_WITH_OGONEK #x3E7)
(define-keysym #\LATIN_SMALL_LETTER_E_WITH_DOT_ABOVE #x3EC)
(define-keysym #\LATIN_SMALL_LETTER_I_WITH_MACRON #x3EF)
(define-keysym #\LATIN_SMALL_LETTER_N_WITH_CEDILLA #x3F1)
(define-keysym #\LATIN_SMALL_LETTER_O_WITH_MACRON #x3F2)
(define-keysym #\LATIN_SMALL_LETTER_K_WITH_CEDILLA #x3F3)
(define-keysym #\LATIN_SMALL_LETTER_U_WITH_OGONEK #x3F9)
(define-keysym #\LATIN_SMALL_LETTER_U_WITH_TILDE #x3FD)
(define-keysym #\LATIN_SMALL_LETTER_U_WITH_MACRON #x3FE)
(define-keysym #\LATIN_CAPITAL_LIGATURE_OE #x13BC)
(define-keysym #\LATIN_SMALL_LIGATURE_OE #x13BD)
(define-keysym #\LATIN_CAPITAL_LETTER_Y_WITH_DIAERESIS #x13BE)
(define-keysym #\OVERLINE #x47E)
(define-keysym #\IDEOGRAPHIC_FULL_STOP #x4A1)
(define-keysym #\LEFT_CORNER_BRACKET #x4A2)
(define-keysym #\RIGHT_CORNER_BRACKET #x4A3)
(define-keysym #\IDEOGRAPHIC_COMMA #x4A4)
(define-keysym #\KATAKANA_MIDDLE_DOT #x4A5)
(define-keysym #\KATAKANA_LETTER_WO #x4A6)
(define-keysym #\KATAKANA_LETTER_SMALL_A #x4A7)
(define-keysym #\KATAKANA_LETTER_SMALL_I #x4A8)
(define-keysym #\KATAKANA_LETTER_SMALL_U #x4A9)
(define-keysym #\KATAKANA_LETTER_SMALL_E #x4AA)
(define-keysym #\KATAKANA_LETTER_SMALL_O #x4AB)
(define-keysym #\KATAKANA_LETTER_SMALL_YA #x4AC)
(define-keysym #\KATAKANA_LETTER_SMALL_YU #x4AD)
(define-keysym #\KATAKANA_LETTER_SMALL_YO #x4AE)
(define-keysym #\KATAKANA_LETTER_SMALL_TU #x4AF)
(define-keysym #\KATAKANA-HIRAGANA_PROLONGED_SOUND_MARK #x4B0)
(define-keysym #\KATAKANA_LETTER_A #x4B1)
(define-keysym #\KATAKANA_LETTER_I #x4B2)
(define-keysym #\KATAKANA_LETTER_U #x4B3)
(define-keysym #\KATAKANA_LETTER_E #x4B4)
(define-keysym #\KATAKANA_LETTER_O #x4B5)
(define-keysym #\KATAKANA_LETTER_KA #x4B6)
(define-keysym #\KATAKANA_LETTER_KI #x4B7)
(define-keysym #\KATAKANA_LETTER_KU #x4B8)
(define-keysym #\KATAKANA_LETTER_KE #x4B9)
(define-keysym #\KATAKANA_LETTER_KO #x4BA)
(define-keysym #\KATAKANA_LETTER_SA #x4BB)
(define-keysym #\KATAKANA_LETTER_SI #x4BC)
(define-keysym #\KATAKANA_LETTER_SU #x4BD)
(define-keysym #\KATAKANA_LETTER_SE #x4BE)
(define-keysym #\KATAKANA_LETTER_SO #x4BF)
(define-keysym #\KATAKANA_LETTER_TA #x4C0)
(define-keysym #\KATAKANA_LETTER_TI #x4C1)
(define-keysym #\KATAKANA_LETTER_TU #x4C2)
(define-keysym #\KATAKANA_LETTER_TE #x4C3)
(define-keysym #\KATAKANA_LETTER_TO #x4C4)
(define-keysym #\KATAKANA_LETTER_NA #x4C5)
(define-keysym #\KATAKANA_LETTER_NI #x4C6)
(define-keysym #\KATAKANA_LETTER_NU #x4C7)
(define-keysym #\KATAKANA_LETTER_NE #x4C8)
(define-keysym #\KATAKANA_LETTER_NO #x4C9)
(define-keysym #\KATAKANA_LETTER_HA #x4CA)
(define-keysym #\KATAKANA_LETTER_HI #x4CB)
(define-keysym #\KATAKANA_LETTER_HU #x4CC)
(define-keysym #\KATAKANA_LETTER_HE #x4CD)
(define-keysym #\KATAKANA_LETTER_HO #x4CE)
(define-keysym #\KATAKANA_LETTER_MA #x4CF)
(define-keysym #\KATAKANA_LETTER_MI #x4D0)
(define-keysym #\KATAKANA_LETTER_MU #x4D1)
(define-keysym #\KATAKANA_LETTER_ME #x4D2)
(define-keysym #\KATAKANA_LETTER_MO #x4D3)
(define-keysym #\KATAKANA_LETTER_YA #x4D4)
(define-keysym #\KATAKANA_LETTER_YU #x4D5)
(define-keysym #\KATAKANA_LETTER_YO #x4D6)
(define-keysym #\KATAKANA_LETTER_RA #x4D7)
(define-keysym #\KATAKANA_LETTER_RI #x4D8)
(define-keysym #\KATAKANA_LETTER_RU #x4D9)
(define-keysym #\KATAKANA_LETTER_RE #x4DA)
(define-keysym #\KATAKANA_LETTER_RO #x4DB)
(define-keysym #\KATAKANA_LETTER_WA #x4DC)
(define-keysym #\KATAKANA_LETTER_N #x4DD)
(define-keysym #\KATAKANA-HIRAGANA_VOICED_SOUND_MARK #x4DE)
(define-keysym #\KATAKANA-HIRAGANA_SEMI-VOICED_SOUND_MARK #x4DF)
(define-keysym :|kana_switch| #xFF7E)
(define-keysym #\ARABIC_COMMA #x5AC)
(define-keysym #\ARABIC_SEMICOLON #x5BB)
(define-keysym #\ARABIC_QUESTION_MARK #x5BF)
(define-keysym #\ARABIC_LETTER_HAMZA #x5C1)
(define-keysym #\ARABIC_LETTER_ALEF_WITH_MADDA_ABOVE #x5C2)
(define-keysym #\ARABIC_LETTER_ALEF_WITH_HAMZA_ABOVE #x5C3)
(define-keysym #\ARABIC_LETTER_WAW_WITH_HAMZA_ABOVE #x5C4)
(define-keysym #\ARABIC_LETTER_ALEF_WITH_HAMZA_BELOW #x5C5)
(define-keysym #\ARABIC_LETTER_YEH_WITH_HAMZA_ABOVE #x5C6)
(define-keysym #\ARABIC_LETTER_ALEF #x5C7)
(define-keysym #\ARABIC_LETTER_BEH #x5C8)
(define-keysym #\ARABIC_LETTER_TEH_MARBUTA #x5C9)
(define-keysym #\ARABIC_LETTER_TEH #x5CA)
(define-keysym #\ARABIC_LETTER_THEH #x5CB)
(define-keysym #\ARABIC_LETTER_JEEM #x5CC)
(define-keysym #\ARABIC_LETTER_HAH #x5CD)
(define-keysym #\ARABIC_LETTER_KHAH #x5CE)
(define-keysym #\ARABIC_LETTER_DAL #x5CF)
(define-keysym #\ARABIC_LETTER_THAL #x5D0)
(define-keysym #\ARABIC_LETTER_REH #x5D1)
(define-keysym #\ARABIC_LETTER_ZAIN #x5D2)
(define-keysym #\ARABIC_LETTER_SEEN #x5D3)
(define-keysym #\ARABIC_LETTER_SHEEN #x5D4)
(define-keysym #\ARABIC_LETTER_SAD #x5D5)
(define-keysym #\ARABIC_LETTER_DAD #x5D6)
(define-keysym #\ARABIC_LETTER_TAH #x5D7)
(define-keysym #\ARABIC_LETTER_ZAH #x5D8)
(define-keysym #\ARABIC_LETTER_AIN #x5D9)
(define-keysym #\ARABIC_LETTER_GHAIN #x5DA)
(define-keysym #\ARABIC_TATWEEL #x5E0)
(define-keysym #\ARABIC_LETTER_FEH #x5E1)
(define-keysym #\ARABIC_LETTER_QAF #x5E2)
(define-keysym #\ARABIC_LETTER_KAF #x5E3)
(define-keysym #\ARABIC_LETTER_LAM #x5E4)
(define-keysym #\ARABIC_LETTER_MEEM #x5E5)
(define-keysym #\ARABIC_LETTER_NOON #x5E6)
(define-keysym #\ARABIC_LETTER_HEH #x5E7)
(define-keysym #\ARABIC_LETTER_WAW #x5E8)
(define-keysym #\ARABIC_LETTER_ALEF_MAKSURA #x5E9)
(define-keysym #\ARABIC_LETTER_YEH #x5EA)
(define-keysym #\ARABIC_FATHATAN #x5EB)
(define-keysym #\ARABIC_DAMMATAN #x5EC)
(define-keysym #\ARABIC_KASRATAN #x5ED)
(define-keysym #\ARABIC_FATHA #x5EE)
(define-keysym #\ARABIC_DAMMA #x5EF)
(define-keysym #\ARABIC_KASRA #x5F0)
(define-keysym #\ARABIC_SHADDA #x5F1)
(define-keysym #\ARABIC_SUKUN #x5F2)
(define-keysym :|Arabic_switch| #xFF7E)
(define-keysym #\CYRILLIC_SMALL_LETTER_DJE #x6A1)
(define-keysym #\CYRILLIC_SMALL_LETTER_GJE #x6A2)
(define-keysym #\CYRILLIC_SMALL_LETTER_IO #x6A3)
(define-keysym #\CYRILLIC_SMALL_LETTER_UKRAINIAN_IE #x6A4)
(define-keysym #\CYRILLIC_SMALL_LETTER_DZE #x6A5)
(define-keysym #\CYRILLIC_SMALL_LETTER_BYELORUSSIAN-UKRAINIAN_I #x6A6)
(define-keysym #\CYRILLIC_SMALL_LETTER_YI #x6A7)
(define-keysym #\CYRILLIC_SMALL_LETTER_JE #x6A8)
(define-keysym #\CYRILLIC_SMALL_LETTER_LJE #x6A9)
(define-keysym #\CYRILLIC_SMALL_LETTER_NJE #x6AA)
(define-keysym #\CYRILLIC_SMALL_LETTER_TSHE #x6AB)
(define-keysym #\CYRILLIC_SMALL_LETTER_KJE #x6AC)
(define-keysym #\CYRILLIC_SMALL_LETTER_GHE_WITH_UPTURN #x6AD)
(define-keysym #\CYRILLIC_SMALL_LETTER_SHORT_U #x6AE)
(define-keysym #\CYRILLIC_SMALL_LETTER_DZHE #x6AF)
(define-keysym #\NUMERO_SIGN #x6B0)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_DJE #x6B1)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_GJE #x6B2)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_IO #x6B3)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_UKRAINIAN_IE #x6B4)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_DZE #x6B5)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_BYELORUSSIAN-UKRAINIAN_I #x6B6)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_YI #x6B7)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_JE #x6B8)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_LJE #x6B9)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_NJE #x6BA)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_TSHE #x6BB)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_KJE #x6BC)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_GHE_WITH_UPTURN #x6BD)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_SHORT_U #x6BE)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_DZHE #x6BF)
(define-keysym #\CYRILLIC_SMALL_LETTER_YU #x6C0)
(define-keysym #\CYRILLIC_SMALL_LETTER_A #x6C1)
(define-keysym #\CYRILLIC_SMALL_LETTER_BE #x6C2)
(define-keysym #\CYRILLIC_SMALL_LETTER_TSE #x6C3)
(define-keysym #\CYRILLIC_SMALL_LETTER_DE #x6C4)
(define-keysym #\CYRILLIC_SMALL_LETTER_IE #x6C5)
(define-keysym #\CYRILLIC_SMALL_LETTER_EF #x6C6)
(define-keysym #\CYRILLIC_SMALL_LETTER_GHE #x6C7)
(define-keysym #\CYRILLIC_SMALL_LETTER_HA #x6C8)
(define-keysym #\CYRILLIC_SMALL_LETTER_I #x6C9)
(define-keysym #\CYRILLIC_SMALL_LETTER_SHORT_I #x6CA)
(define-keysym #\CYRILLIC_SMALL_LETTER_KA #x6CB)
(define-keysym #\CYRILLIC_SMALL_LETTER_EL #x6CC)
(define-keysym #\CYRILLIC_SMALL_LETTER_EM #x6CD)
(define-keysym #\CYRILLIC_SMALL_LETTER_EN #x6CE)
(define-keysym #\CYRILLIC_SMALL_LETTER_O #x6CF)
(define-keysym #\CYRILLIC_SMALL_LETTER_PE #x6D0)
(define-keysym #\CYRILLIC_SMALL_LETTER_YA #x6D1)
(define-keysym #\CYRILLIC_SMALL_LETTER_ER #x6D2)
(define-keysym #\CYRILLIC_SMALL_LETTER_ES #x6D3)
(define-keysym #\CYRILLIC_SMALL_LETTER_TE #x6D4)
(define-keysym #\CYRILLIC_SMALL_LETTER_U #x6D5)
(define-keysym #\CYRILLIC_SMALL_LETTER_ZHE #x6D6)
(define-keysym #\CYRILLIC_SMALL_LETTER_VE #x6D7)
(define-keysym #\CYRILLIC_SMALL_LETTER_SOFT_SIGN #x6D8)
(define-keysym #\CYRILLIC_SMALL_LETTER_YERU #x6D9)
(define-keysym #\CYRILLIC_SMALL_LETTER_ZE #x6DA)
(define-keysym #\CYRILLIC_SMALL_LETTER_SHA #x6DB)
(define-keysym #\CYRILLIC_SMALL_LETTER_E #x6DC)
(define-keysym #\CYRILLIC_SMALL_LETTER_SHCHA #x6DD)
(define-keysym #\CYRILLIC_SMALL_LETTER_CHE #x6DE)
(define-keysym #\CYRILLIC_SMALL_LETTER_HARD_SIGN #x6DF)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_YU #x6E0)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_A #x6E1)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_BE #x6E2)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_TSE #x6E3)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_DE #x6E4)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_IE #x6E5)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_EF #x6E6)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_GHE #x6E7)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_HA #x6E8)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_I #x6E9)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_SHORT_I #x6EA)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_KA #x6EB)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_EL #x6EC)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_EM #x6ED)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_EN #x6EE)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_O #x6EF)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_PE #x6F0)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_YA #x6F1)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_ER #x6F2)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_ES #x6F3)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_TE #x6F4)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_U #x6F5)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_ZHE #x6F6)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_VE #x6F7)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_SOFT_SIGN #x6F8)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_YERU #x6F9)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_ZE #x6FA)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_SHA #x6FB)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_E #x6FC)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_SHCHA #x6FD)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_CHE #x6FE)
(define-keysym #\CYRILLIC_CAPITAL_LETTER_HARD_SIGN #x6FF)
(define-keysym #\GREEK_CAPITAL_LETTER_ALPHA_WITH_TONOS #x7A1)
(define-keysym #\GREEK_CAPITAL_LETTER_EPSILON_WITH_TONOS #x7A2)
(define-keysym #\GREEK_CAPITAL_LETTER_ETA_WITH_TONOS #x7A3)
(define-keysym #\GREEK_CAPITAL_LETTER_IOTA_WITH_TONOS #x7A4)
(define-keysym #\GREEK_CAPITAL_LETTER_IOTA_WITH_DIALYTIKA #x7A5)
(define-keysym #\GREEK_CAPITAL_LETTER_OMICRON_WITH_TONOS #x7A7)
(define-keysym #\GREEK_CAPITAL_LETTER_UPSILON_WITH_TONOS #x7A8)
(define-keysym #\GREEK_CAPITAL_LETTER_UPSILON_WITH_DIALYTIKA #x7A9)
(define-keysym #\GREEK_CAPITAL_LETTER_OMEGA_WITH_TONOS #x7AB)
(define-keysym #\GREEK_DIALYTIKA_TONOS #x7AE)
(define-keysym #\HORIZONTAL_BAR #x7AF)
(define-keysym #\GREEK_SMALL_LETTER_ALPHA_WITH_TONOS #x7B1)
(define-keysym #\GREEK_SMALL_LETTER_EPSILON_WITH_TONOS #x7B2)
(define-keysym #\GREEK_SMALL_LETTER_ETA_WITH_TONOS #x7B3)
(define-keysym #\GREEK_SMALL_LETTER_IOTA_WITH_TONOS #x7B4)
(define-keysym #\GREEK_SMALL_LETTER_IOTA_WITH_DIALYTIKA #x7B5)
(define-keysym #\GREEK_SMALL_LETTER_IOTA_WITH_DIALYTIKA_AND_TONOS #x7B6)
(define-keysym #\GREEK_SMALL_LETTER_OMICRON_WITH_TONOS #x7B7)
(define-keysym #\GREEK_SMALL_LETTER_UPSILON_WITH_TONOS #x7B8)
(define-keysym #\GREEK_SMALL_LETTER_UPSILON_WITH_DIALYTIKA #x7B9)
(define-keysym #\GREEK_SMALL_LETTER_UPSILON_WITH_DIALYTIKA_AND_TONOS #x7BA)
(define-keysym #\GREEK_SMALL_LETTER_OMEGA_WITH_TONOS #x7BB)
(define-keysym #\GREEK_CAPITAL_LETTER_ALPHA #x7C1)
(define-keysym #\GREEK_CAPITAL_LETTER_BETA #x7C2)
(define-keysym #\GREEK_CAPITAL_LETTER_GAMMA #x7C3)
(define-keysym #\GREEK_CAPITAL_LETTER_DELTA #x7C4)
(define-keysym #\GREEK_CAPITAL_LETTER_EPSILON #x7C5)
(define-keysym #\GREEK_CAPITAL_LETTER_ZETA #x7C6)
(define-keysym #\GREEK_CAPITAL_LETTER_ETA #x7C7)
(define-keysym #\GREEK_CAPITAL_LETTER_THETA #x7C8)
(define-keysym #\GREEK_CAPITAL_LETTER_IOTA #x7C9)
(define-keysym #\GREEK_CAPITAL_LETTER_KAPPA #x7CA)
(define-keysym #\GREEK_CAPITAL_LETTER_LAMDA #x7CB)
(define-keysym #\GREEK_CAPITAL_LETTER_LAMDA #x7CB)
(define-keysym #\GREEK_CAPITAL_LETTER_MU #x7CC)
(define-keysym #\GREEK_CAPITAL_LETTER_NU #x7CD)
(define-keysym #\GREEK_CAPITAL_LETTER_XI #x7CE)
(define-keysym #\GREEK_CAPITAL_LETTER_OMICRON #x7CF)
(define-keysym #\GREEK_CAPITAL_LETTER_PI #x7D0)
(define-keysym #\GREEK_CAPITAL_LETTER_RHO #x7D1)
(define-keysym #\GREEK_CAPITAL_LETTER_SIGMA #x7D2)
(define-keysym #\GREEK_CAPITAL_LETTER_TAU #x7D4)
(define-keysym #\GREEK_CAPITAL_LETTER_UPSILON #x7D5)
(define-keysym #\GREEK_CAPITAL_LETTER_PHI #x7D6)
(define-keysym #\GREEK_CAPITAL_LETTER_CHI #x7D7)
(define-keysym #\GREEK_CAPITAL_LETTER_PSI #x7D8)
(define-keysym #\GREEK_CAPITAL_LETTER_OMEGA #x7D9)
(define-keysym #\GREEK_SMALL_LETTER_ALPHA #x7E1)
(define-keysym #\GREEK_SMALL_LETTER_BETA #x7E2)
(define-keysym #\GREEK_SMALL_LETTER_GAMMA #x7E3)
(define-keysym #\GREEK_SMALL_LETTER_DELTA #x7E4)
(define-keysym #\GREEK_SMALL_LETTER_EPSILON #x7E5)
(define-keysym #\GREEK_SMALL_LETTER_ZETA #x7E6)
(define-keysym #\GREEK_SMALL_LETTER_ETA #x7E7)
(define-keysym #\GREEK_SMALL_LETTER_THETA #x7E8)
(define-keysym #\GREEK_SMALL_LETTER_IOTA #x7E9)
(define-keysym #\GREEK_SMALL_LETTER_KAPPA #x7EA)
(define-keysym #\GREEK_SMALL_LETTER_LAMDA #x7EB)
(define-keysym #\GREEK_SMALL_LETTER_LAMDA #x7EB)
(define-keysym #\GREEK_SMALL_LETTER_MU #x7EC)
(define-keysym #\GREEK_SMALL_LETTER_NU #x7ED)
(define-keysym #\GREEK_SMALL_LETTER_XI #x7EE)
(define-keysym #\GREEK_SMALL_LETTER_OMICRON #x7EF)
(define-keysym #\GREEK_SMALL_LETTER_PI #x7F0)
(define-keysym #\GREEK_SMALL_LETTER_RHO #x7F1)
(define-keysym #\GREEK_SMALL_LETTER_SIGMA #x7F2)
(define-keysym #\GREEK_SMALL_LETTER_FINAL_SIGMA #x7F3)
(define-keysym #\GREEK_SMALL_LETTER_TAU #x7F4)
(define-keysym #\GREEK_SMALL_LETTER_UPSILON #x7F5)
(define-keysym #\GREEK_SMALL_LETTER_PHI #x7F6)
(define-keysym #\GREEK_SMALL_LETTER_CHI #x7F7)
(define-keysym #\GREEK_SMALL_LETTER_PSI #x7F8)
(define-keysym #\GREEK_SMALL_LETTER_OMEGA #x7F9)
(define-keysym :|Greek_switch| #xFF7E)
(define-keysym #\RADICAL_SYMBOL_BOTTOM #x8A1)
(define-keysym #\BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT #x8A2)
(define-keysym #\BOX_DRAWINGS_LIGHT_HORIZONTAL #x8A3)
(define-keysym #\TOP_HALF_INTEGRAL #x8A4)
(define-keysym #\BOTTOM_HALF_INTEGRAL #x8A5)
(define-keysym #\BOX_DRAWINGS_LIGHT_VERTICAL #x8A6)
(define-keysym #\LEFT_SQUARE_BRACKET_UPPER_CORNER #x8A7)
(define-keysym #\LEFT_SQUARE_BRACKET_LOWER_CORNER #x8A8)
(define-keysym #\RIGHT_SQUARE_BRACKET_UPPER_CORNER #x8A9)
(define-keysym #\RIGHT_SQUARE_BRACKET_LOWER_CORNER #x8AA)
(define-keysym #\LEFT_PARENTHESIS_UPPER_HOOK #x8AB)
(define-keysym #\LEFT_PARENTHESIS_LOWER_HOOK #x8AC)
(define-keysym #\RIGHT_PARENTHESIS_UPPER_HOOK #x8AD)
(define-keysym #\RIGHT_PARENTHESIS_LOWER_HOOK #x8AE)
(define-keysym #\LEFT_CURLY_BRACKET_MIDDLE_PIECE #x8AF)
(define-keysym #\RIGHT_CURLY_BRACKET_MIDDLE_PIECE #x8B0)
(define-keysym #\LESS-THAN_OR_EQUAL_TO #x8BC)
(define-keysym #\NOT_EQUAL_TO #x8BD)
(define-keysym #\GREATER-THAN_OR_EQUAL_TO #x8BE)
(define-keysym #\INTEGRAL #x8BF)
(define-keysym #\THEREFORE #x8C0)
(define-keysym #\PROPORTIONAL_TO #x8C1)
(define-keysym #\INFINITY #x8C2)
(define-keysym #\NABLA #x8C5)
(define-keysym #\TILDE_OPERATOR #x8C8)
(define-keysym #\ASYMPTOTICALLY_EQUAL_TO #x8C9)
(define-keysym #\LEFT_RIGHT_DOUBLE_ARROW #x8CD)
(define-keysym #\RIGHTWARDS_DOUBLE_ARROW #x8CE)
(define-keysym #\IDENTICAL_TO #x8CF)
(define-keysym #\SQUARE_ROOT #x8D6)
(define-keysym #\SUBSET_OF #x8DA)
(define-keysym #\SUPERSET_OF #x8DB)
(define-keysym #\INTERSECTION #x8DC)
(define-keysym #\UNION #x8DD)
(define-keysym #\LOGICAL_AND #x8DE)
(define-keysym #\LOGICAL_OR #x8DF)
(define-keysym #\PARTIAL_DIFFERENTIAL #x8EF)
(define-keysym #\LATIN_SMALL_LETTER_F_WITH_HOOK #x8F6)
(define-keysym #\LEFTWARDS_ARROW #x8FB)
(define-keysym #\UPWARDS_ARROW #x8FC)
(define-keysym #\RIGHTWARDS_ARROW #x8FD)
(define-keysym #\DOWNWARDS_ARROW #x8FE)
(define-keysym #\BLACK_DIAMOND #x9E0)
(define-keysym #\MEDIUM_SHADE #x9E1)
(define-keysym #\SYMBOL_FOR_HORIZONTAL_TABULATION #x9E2)
(define-keysym #\SYMBOL_FOR_FORM_FEED #x9E3)
(define-keysym #\SYMBOL_FOR_CARRIAGE_RETURN #x9E4)
(define-keysym #\SYMBOL_FOR_LINE_FEED #x9E5)
(define-keysym #\SYMBOL_FOR_NEWLINE #x9E8)
(define-keysym #\SYMBOL_FOR_VERTICAL_TABULATION #x9E9)
(define-keysym #\BOX_DRAWINGS_LIGHT_UP_AND_LEFT #x9EA)
(define-keysym #\BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT #x9EB)
(define-keysym #\BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT #x9EC)
(define-keysym #\BOX_DRAWINGS_LIGHT_UP_AND_RIGHT #x9ED)
(define-keysym #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL #x9EE)
(define-keysym #\HORIZONTAL_SCAN_LINE-1 #x9EF)
(define-keysym #\HORIZONTAL_SCAN_LINE-3 #x9F0)
(define-keysym #\BOX_DRAWINGS_LIGHT_HORIZONTAL #x9F1)
(define-keysym #\HORIZONTAL_SCAN_LINE-7 #x9F2)
(define-keysym #\HORIZONTAL_SCAN_LINE-9 #x9F3)
(define-keysym #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT #x9F4)
(define-keysym #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT #x9F5)
(define-keysym #\BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL #x9F6)
(define-keysym #\BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL #x9F7)
(define-keysym #\BOX_DRAWINGS_LIGHT_VERTICAL #x9F8)
(define-keysym #\EM_SPACE #xAA1)
(define-keysym #\EN_SPACE #xAA2)
(define-keysym #\THREE-PER-EM_SPACE #xAA3)
(define-keysym #\FOUR-PER-EM_SPACE #xAA4)
(define-keysym #\FIGURE_SPACE #xAA5)
(define-keysym #\PUNCTUATION_SPACE #xAA6)
(define-keysym #\THIN_SPACE #xAA7)
(define-keysym #\HAIR_SPACE #xAA8)
(define-keysym #\EM_DASH #xAA9)
(define-keysym #\EN_DASH #xAAA)
(define-keysym #\OPEN_BOX #xAAC)
(define-keysym #\HORIZONTAL_ELLIPSIS #xAAE)
(define-keysym #\TWO_DOT_LEADER #xAAF)
(define-keysym #\VULGAR_FRACTION_ONE_THIRD #xAB0)
(define-keysym #\VULGAR_FRACTION_TWO_THIRDS #xAB1)
(define-keysym #\VULGAR_FRACTION_ONE_FIFTH #xAB2)
(define-keysym #\VULGAR_FRACTION_TWO_FIFTHS #xAB3)
(define-keysym #\VULGAR_FRACTION_THREE_FIFTHS #xAB4)
(define-keysym #\VULGAR_FRACTION_FOUR_FIFTHS #xAB5)
(define-keysym #\VULGAR_FRACTION_ONE_SIXTH #xAB6)
(define-keysym #\VULGAR_FRACTION_FIVE_SIXTHS #xAB7)
(define-keysym #\CARE_OF #xAB8)
(define-keysym #\FIGURE_DASH #xABB)
(define-keysym #\MATHEMATICAL_LEFT_ANGLE_BRACKET #xABC)
(define-keysym #\. #xABD)
(define-keysym #\MATHEMATICAL_RIGHT_ANGLE_BRACKET #xABE)
(define-keysym #\VULGAR_FRACTION_ONE_EIGHTH #xAC3)
(define-keysym #\VULGAR_FRACTION_THREE_EIGHTHS #xAC4)
(define-keysym #\VULGAR_FRACTION_FIVE_EIGHTHS #xAC5)
(define-keysym #\VULGAR_FRACTION_SEVEN_EIGHTHS #xAC6)
(define-keysym #\TRADE_MARK_SIGN #xAC9)
(define-keysym #\SALTIRE #xACA)
(define-keysym #\WHITE_LEFT-POINTING_TRIANGLE #xACC)
(define-keysym #\WHITE_RIGHT-POINTING_TRIANGLE #xACD)
(define-keysym #\WHITE_CIRCLE #xACE)
(define-keysym #\WHITE_VERTICAL_RECTANGLE #xACF)
(define-keysym #\LEFT_SINGLE_QUOTATION_MARK #xAD0)
(define-keysym #\RIGHT_SINGLE_QUOTATION_MARK #xAD1)
(define-keysym #\LEFT_DOUBLE_QUOTATION_MARK #xAD2)
(define-keysym #\RIGHT_DOUBLE_QUOTATION_MARK #xAD3)
(define-keysym #\PRESCRIPTION_TAKE #xAD4)
(define-keysym #\PRIME #xAD6)
(define-keysym #\DOUBLE_PRIME #xAD7)
(define-keysym #\LATIN_CROSS #xAD9)
(define-keysym #\BLACK_RECTANGLE #xADB)
(define-keysym #\BLACK_LEFT-POINTING_TRIANGLE #xADC)
(define-keysym #\BLACK_RIGHT-POINTING_TRIANGLE #xADD)
(define-keysym #\BLACK_CIRCLE #xADE)
(define-keysym #\BLACK_VERTICAL_RECTANGLE #xADF)
(define-keysym #\WHITE_BULLET #xAE0)
(define-keysym #\WHITE_SMALL_SQUARE #xAE1)
(define-keysym #\WHITE_RECTANGLE #xAE2)
(define-keysym #\WHITE_UP-POINTING_TRIANGLE #xAE3)
(define-keysym #\WHITE_DOWN-POINTING_TRIANGLE #xAE4)
(define-keysym #\WHITE_STAR #xAE5)
(define-keysym #\BULLET #xAE6)
(define-keysym #\BLACK_SMALL_SQUARE #xAE7)
(define-keysym #\BLACK_UP-POINTING_TRIANGLE #xAE8)
(define-keysym #\BLACK_DOWN-POINTING_TRIANGLE #xAE9)
(define-keysym #\WHITE_LEFT_POINTING_INDEX #xAEA)
(define-keysym #\WHITE_RIGHT_POINTING_INDEX #xAEB)
(define-keysym #\BLACK_CLUB_SUIT #xAEC)
(define-keysym #\BLACK_DIAMOND_SUIT #xAED)
(define-keysym #\BLACK_HEART_SUIT #xAEE)
(define-keysym #\MALTESE_CROSS #xAF0)
(define-keysym #\DAGGER #xAF1)
(define-keysym #\DOUBLE_DAGGER #xAF2)
(define-keysym #\CHECK_MARK #xAF3)
(define-keysym #\BALLOT_X #xAF4)
(define-keysym #\MUSIC_SHARP_SIGN #xAF5)
(define-keysym #\MUSIC_FLAT_SIGN #xAF6)
(define-keysym #\MALE_SIGN #xAF7)
(define-keysym #\FEMALE_SIGN #xAF8)
(define-keysym #\BLACK_TELEPHONE #xAF9)
(define-keysym #\TELEPHONE_RECORDER #xAFA)
(define-keysym #\SOUND_RECORDING_COPYRIGHT #xAFB)
(define-keysym #\CARET #xAFC)
(define-keysym #\SINGLE_LOW-9_QUOTATION_MARK #xAFD)
(define-keysym #\DOUBLE_LOW-9_QUOTATION_MARK #xAFE)
(define-keysym #\< #xBA3)
(define-keysym #\> #xBA6)
(define-keysym #\LOGICAL_OR #xBA8)
(define-keysym #\LOGICAL_AND #xBA9)
(define-keysym #\MACRON #xBC0)
(define-keysym #\DOWN_TACK #xBC2)
(define-keysym #\INTERSECTION #xBC3)
(define-keysym #\LEFT_FLOOR #xBC4)
(define-keysym #\_ #xBC6)
(define-keysym #\RING_OPERATOR #xBCA)
(define-keysym #\APL_FUNCTIONAL_SYMBOL_QUAD #xBCC)
(define-keysym #\UP_TACK #xBCE)
(define-keysym #\WHITE_CIRCLE #xBCF)
(define-keysym #\LEFT_CEILING #xBD3)
(define-keysym #\UNION #xBD6)
(define-keysym #\SUPERSET_OF #xBD8)
(define-keysym #\SUBSET_OF #xBDA)
(define-keysym #\LEFT_TACK #xBDC)
(define-keysym #\RIGHT_TACK #xBFC)
(define-keysym #\DOUBLE_LOW_LINE #xCDF)
(define-keysym #\HEBREW_LETTER_ALEF #xCE0)
(define-keysym #\HEBREW_LETTER_BET #xCE1)
(define-keysym #\HEBREW_LETTER_GIMEL #xCE2)
(define-keysym #\HEBREW_LETTER_DALET #xCE3)
(define-keysym #\HEBREW_LETTER_HE #xCE4)
(define-keysym #\HEBREW_LETTER_VAV #xCE5)
(define-keysym #\HEBREW_LETTER_ZAYIN #xCE6)
(define-keysym #\HEBREW_LETTER_HET #xCE7)
(define-keysym #\HEBREW_LETTER_TET #xCE8)
(define-keysym #\HEBREW_LETTER_YOD #xCE9)
(define-keysym #\HEBREW_LETTER_FINAL_KAF #xCEA)
(define-keysym #\HEBREW_LETTER_KAF #xCEB)
(define-keysym #\HEBREW_LETTER_LAMED #xCEC)
(define-keysym #\HEBREW_LETTER_FINAL_MEM #xCED)
(define-keysym #\HEBREW_LETTER_MEM #xCEE)
(define-keysym #\HEBREW_LETTER_FINAL_NUN #xCEF)
(define-keysym #\HEBREW_LETTER_NUN #xCF0)
(define-keysym #\HEBREW_LETTER_SAMEKH #xCF1)
(define-keysym #\HEBREW_LETTER_AYIN #xCF2)
(define-keysym #\HEBREW_LETTER_FINAL_PE #xCF3)
(define-keysym #\HEBREW_LETTER_PE #xCF4)
(define-keysym #\HEBREW_LETTER_FINAL_TSADI #xCF5)
(define-keysym #\HEBREW_LETTER_TSADI #xCF6)
(define-keysym #\HEBREW_LETTER_QOF #xCF7)
(define-keysym #\HEBREW_LETTER_RESH #xCF8)
(define-keysym #\HEBREW_LETTER_SHIN #xCF9)
(define-keysym #\HEBREW_LETTER_TAV #xCFA)
(define-keysym :|Hebrew_switch| #xFF7E)
(define-keysym #\THAI_CHARACTER_KO_KAI #xDA1)
(define-keysym #\THAI_CHARACTER_KHO_KHAI #xDA2)
(define-keysym #\THAI_CHARACTER_KHO_KHUAT #xDA3)
(define-keysym #\THAI_CHARACTER_KHO_KHWAI #xDA4)
(define-keysym #\THAI_CHARACTER_KHO_KHON #xDA5)
(define-keysym #\THAI_CHARACTER_KHO_RAKHANG #xDA6)
(define-keysym #\THAI_CHARACTER_NGO_NGU #xDA7)
(define-keysym #\THAI_CHARACTER_CHO_CHAN #xDA8)
(define-keysym #\THAI_CHARACTER_CHO_CHING #xDA9)
(define-keysym #\THAI_CHARACTER_CHO_CHANG #xDAA)
(define-keysym #\THAI_CHARACTER_SO_SO #xDAB)
(define-keysym #\THAI_CHARACTER_CHO_CHOE #xDAC)
(define-keysym #\THAI_CHARACTER_YO_YING #xDAD)
(define-keysym #\THAI_CHARACTER_DO_CHADA #xDAE)
(define-keysym #\THAI_CHARACTER_TO_PATAK #xDAF)
(define-keysym #\THAI_CHARACTER_THO_THAN #xDB0)
(define-keysym #\THAI_CHARACTER_THO_NANGMONTHO #xDB1)
(define-keysym #\THAI_CHARACTER_THO_PHUTHAO #xDB2)
(define-keysym #\THAI_CHARACTER_NO_NEN #xDB3)
(define-keysym #\THAI_CHARACTER_DO_DEK #xDB4)
(define-keysym #\THAI_CHARACTER_TO_TAO #xDB5)
(define-keysym #\THAI_CHARACTER_THO_THUNG #xDB6)
(define-keysym #\THAI_CHARACTER_THO_THAHAN #xDB7)
(define-keysym #\THAI_CHARACTER_THO_THONG #xDB8)
(define-keysym #\THAI_CHARACTER_NO_NU #xDB9)
(define-keysym #\THAI_CHARACTER_BO_BAIMAI #xDBA)
(define-keysym #\THAI_CHARACTER_PO_PLA #xDBB)
(define-keysym #\THAI_CHARACTER_PHO_PHUNG #xDBC)
(define-keysym #\THAI_CHARACTER_FO_FA #xDBD)
(define-keysym #\THAI_CHARACTER_PHO_PHAN #xDBE)
(define-keysym #\THAI_CHARACTER_FO_FAN #xDBF)
(define-keysym #\THAI_CHARACTER_PHO_SAMPHAO #xDC0)
(define-keysym #\THAI_CHARACTER_MO_MA #xDC1)
(define-keysym #\THAI_CHARACTER_YO_YAK #xDC2)
(define-keysym #\THAI_CHARACTER_RO_RUA #xDC3)
(define-keysym #\THAI_CHARACTER_RU #xDC4)
(define-keysym #\THAI_CHARACTER_LO_LING #xDC5)
(define-keysym #\THAI_CHARACTER_LU #xDC6)
(define-keysym #\THAI_CHARACTER_WO_WAEN #xDC7)
(define-keysym #\THAI_CHARACTER_SO_SALA #xDC8)
(define-keysym #\THAI_CHARACTER_SO_RUSI #xDC9)
(define-keysym #\THAI_CHARACTER_SO_SUA #xDCA)
(define-keysym #\THAI_CHARACTER_HO_HIP #xDCB)
(define-keysym #\THAI_CHARACTER_LO_CHULA #xDCC)
(define-keysym #\THAI_CHARACTER_O_ANG #xDCD)
(define-keysym #\THAI_CHARACTER_HO_NOKHUK #xDCE)
(define-keysym #\THAI_CHARACTER_PAIYANNOI #xDCF)
(define-keysym #\THAI_CHARACTER_SARA_A #xDD0)
(define-keysym #\THAI_CHARACTER_MAI_HAN-AKAT #xDD1)
(define-keysym #\THAI_CHARACTER_SARA_AA #xDD2)
(define-keysym #\THAI_CHARACTER_SARA_AM #xDD3)
(define-keysym #\THAI_CHARACTER_SARA_I #xDD4)
(define-keysym #\THAI_CHARACTER_SARA_II #xDD5)
(define-keysym #\THAI_CHARACTER_SARA_UE #xDD6)
(define-keysym #\THAI_CHARACTER_SARA_UEE #xDD7)
(define-keysym #\THAI_CHARACTER_SARA_U #xDD8)
(define-keysym #\THAI_CHARACTER_SARA_UU #xDD9)
(define-keysym #\THAI_CHARACTER_PHINTHU #xDDA)
(define-keysym #\THAI_CURRENCY_SYMBOL_BAHT #xDDF)
(define-keysym #\THAI_CHARACTER_SARA_E #xDE0)
(define-keysym #\THAI_CHARACTER_SARA_AE #xDE1)
(define-keysym #\THAI_CHARACTER_SARA_O #xDE2)
(define-keysym #\THAI_CHARACTER_SARA_AI_MAIMUAN #xDE3)
(define-keysym #\THAI_CHARACTER_SARA_AI_MAIMALAI #xDE4)
(define-keysym #\THAI_CHARACTER_LAKKHANGYAO #xDE5)
(define-keysym #\THAI_CHARACTER_MAIYAMOK #xDE6)
(define-keysym #\THAI_CHARACTER_MAITAIKHU #xDE7)
(define-keysym #\THAI_CHARACTER_MAI_EK #xDE8)
(define-keysym #\THAI_CHARACTER_MAI_THO #xDE9)
(define-keysym #\THAI_CHARACTER_MAI_TRI #xDEA)
(define-keysym #\THAI_CHARACTER_MAI_CHATTAWA #xDEB)
(define-keysym #\THAI_CHARACTER_THANTHAKHAT #xDEC)
(define-keysym #\THAI_CHARACTER_NIKHAHIT #xDED)
(define-keysym #\THAI_DIGIT_ZERO #xDF0)
(define-keysym #\THAI_DIGIT_ONE #xDF1)
(define-keysym #\THAI_DIGIT_TWO #xDF2)
(define-keysym #\THAI_DIGIT_THREE #xDF3)
(define-keysym #\THAI_DIGIT_FOUR #xDF4)
(define-keysym #\THAI_DIGIT_FIVE #xDF5)
(define-keysym #\THAI_DIGIT_SIX #xDF6)
(define-keysym #\THAI_DIGIT_SEVEN #xDF7)
(define-keysym #\THAI_DIGIT_EIGHT #xDF8)
(define-keysym #\THAI_DIGIT_NINE #xDF9)
(define-keysym :|Hangul| #xFF31)
(define-keysym :|Hangul_Start| #xFF32)
(define-keysym :|Hangul_End| #xFF33)
(define-keysym :|Hangul_Hanja| #xFF34)
(define-keysym :|Hangul_Jamo| #xFF35)
(define-keysym :|Hangul_Romaja| #xFF36)
(define-keysym :|Hangul_Codeinput| #xFF37)
(define-keysym :|Hangul_Jeonja| #xFF38)
(define-keysym :|Hangul_Banja| #xFF39)
(define-keysym :|Hangul_PreHanja| #xFF3A)
(define-keysym :|Hangul_PostHanja| #xFF3B)
(define-keysym :|Hangul_SingleCandidate| #xFF3C)
(define-keysym :|Hangul_MultipleCandidate| #xFF3D)
(define-keysym :|Hangul_PreviousCandidate| #xFF3E)
(define-keysym :|Hangul_Special| #xFF3F)
(define-keysym :|Hangul_switch| #xFF7E)
(define-keysym #\WON_SIGN #xEFF)
(define-keysym #\CONTAINS_AS_MEMBER #x100220)
(define-keysym #\SQUARE_ROOT #x100221)
(define-keysym #\CUBE_ROOT #x100221)
(define-keysym #\FOURTH_ROOT #x100221)
(define-keysym #\DOUBLE_INTEGRAL #x100222)
(define-keysym #\TRIPLE_INTEGRAL #x100222)
(define-keysym :|braille_dot_1| #xFFF1)
(define-keysym :|braille_dot_2| #xFFF2)
(define-keysym :|braille_dot_3| #xFFF3)
(define-keysym :|braille_dot_4| #xFFF4)
(define-keysym :|braille_dot_5| #xFFF5)
(define-keysym :|braille_dot_6| #xFFF6)
(define-keysym :|braille_dot_7| #xFFF7)
(define-keysym :|braille_dot_8| #xFFF8)
(define-keysym :|braille_dot_9| #xFFF9)
(define-keysym :|braille_dot_10| #xFFFA)
