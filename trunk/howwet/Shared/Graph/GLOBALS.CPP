#include <graph\global.h>
#pragma hdrstop

Marker_type_enum All_markers_list[MAX_MARKERS]
 = {M_square,          M_circle,
    M_up_triangle,     M_down_triangle,
    M_left_triangle,   M_right_triangle,
    M_diamond,         M_none};
Pen_style_enum All_pen_styles_list[MAX_PEN_STYLES]
 = {PS_Null,     PS_Solid,         PS_Dash,
    PS_Dot,      PS_Dash_dot,      PS_Dash_dot_dot};
Brush_style_enum All_brush_styles_list[MAX_BRUSH_STYLES]
 = {BS_Null,        BS_Solid,       BS_BDiagonal,
    BS_Cross,       BS_Diagcross,   BS_FDiagonal,
    BS_Horizontal,  BS_Vertical};
GColour All_colours_list[MAX_COLOURS]
 = {RGB(192, 192, 192),  RGB(  0, 255,   0),
    RGB(255,   0, 255),  RGB(255,   0,   0), RGB(  0,   0,   0),
    RGB(  0, 255, 255),  RGB(255, 255,   0),
    RGB(128, 128, 128),  RGB(  0,   0, 255),    RGB(255, 255, 255)};

 void global_dummy(void)
   {
   All_colours_list[0] = RGB(0, 0, 0);
   }

