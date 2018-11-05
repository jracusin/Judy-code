def deg2HMS( RA ):

   '''Borrowed from Sylvain Baumont 
    http://supernovae.in2p3.fr/~baumont/'''
        
   if(RA<0):
      sign = -1
      ra   = -RA
   else:
      sign = 1
      ra   = RA

   h = int( ra/15. )
   ra -= h*15.
   m = int( ra*4.)
   ra -= m/4.
   s = ra*240.

   if(sign == -1):
      out = '-%02d:%02d:%06.3f'%(h,m,s)
   else: out = '+%02d:%02d:%06.3f'%(h,m,s)
   
   return out

def deg2DMS( DEC ):
    
   '''Borrowed from Sylvain Baumont 
    http://supernovae.in2p3.fr/~baumont/'''
    
   if(DEC<0):
      sign = -1
      dec  = -DEC
   else:
      sign = 1
      dec  = DEC

   d = int( dec )
   dec -= d
   dec *= 100.
   m = int( dec*3./5. )
   dec -= m*5./3.
   s = dec*180./5.

   if(sign == -1):
      out = '-%02d:%02d:%06.3f'%(d,m,s)
   else: out = '+%02d:%02d:%06.3f'%(d,m,s)

   return out

