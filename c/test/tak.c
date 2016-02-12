/* trigraphs are only processed with -trigraph option (by cpp) */

/* with cpp, you can break line within whitespaces */
\

/* but you can not place comment after backslash */

tak(x, y, z)
{
  if (x <= y) 
    return z;
  else 
    /*  backslash is processed by cpp. whitespaces after backslash is ignored (with warnings) */
    re\
turn tak(tak(x-1, y, z),
               tak(y-1, z, x),
               tak(z-1, x, y));
%>
