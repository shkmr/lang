
factr(n)
{
  if (n <= 0)
    return 1;
  else
    return n * factr(n-1);
    
}

facti(n)
{
  auto i, v;
  for (i=1; i <= n; i++)
    v *= i;
  return v;
}

fib(n)
{
  if (n==0) return 0;
  if (n==1) return 1;
  return fib(n-2)+fib(n-1);
}

tak(x, y, z)
{
  if (x <= y) 
    return z;
  else 
    return tak(tak(x-1, y, z),
               tak(y-1, z, x),
               tak(z-1, x, y));
}
