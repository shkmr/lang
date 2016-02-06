tak(x, y, z)
{
  if (x <= y) 
    return z;
  else 
    return tak(tak(x-1, y, z),
               tak(y-1, z, x),
               tak(z-1, x, y));
}
