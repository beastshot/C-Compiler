
func X2(a, b, c: int; c1:char) return bool
{
  var res:bool;
  {
     var x, b:char;
     var y: int;
     b='&';
     a=(y*7)/a-y;
  }
  return res;
}
proc  D33( i,j,k,x:int)
{
    func M2 (l, m, n:int) return bool
    {
       var x, j: bool;
       var k:char;
       k='@';
       i=l+l;
       if((k=='*') || (x!=false) && ( l+m < i)) {
           x=l<m;
       }
       return x;
    }
    {
        var x:char;
        var k:bool;
        k=M2(5,i,j);
    }
    x=k;
}
func B52(i,j,k:int) return int
{
   func  square(t:int)  return int 
   {
       var temp:int;
       temp=t*t;
       return temp;
   }
   var  total:int;
   var bo:bool;
   bo=X2(i, j, k, '^');
   total=square(i+j+k); 
   return total;
}
func F15() return int
{
   var s1 : string[100];
   var s2 : string[100];
   var i, j, cnt : int;
   i=0;
   j=0;
   cnt=0;
   while(i < |s1|)  {
        while(j < |s2| / 2)  {
           if(s1[i] == s2[j])   {
               cnt=cnt*2;
           }
          j=j + 1;
        }
        i=i + 1;
   }
   return cnt;
}
func X21() return int
{
   {
     var x:int;
     var y:int*;
     x=5;
     y=&x;
     x=6;
     if( &x == y && ^y == x ) {
       ^y=9;
     }
     {
       var x:char*;
       var y: string[10];
       var z: char;
       x=&y[5];
       z=^(x - 5);
       y="barfoo";
       {
          var x:char;
          var y:int*;
          var z:char*;
          var g:char;
          /% y=&(1+3); 
               y=&x;
               z=&(&(g)); %/
       }
     }
   }
   return 0;
} 
proc Main()
{
  var a:int;
  a = F15();
}
 


  



