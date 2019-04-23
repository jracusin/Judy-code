def deg2HMS( RA0 ):

   '''Borrowed from Sylvain Baumont 
    http://supernovae.in2p3.fr/~baumont/'''
        
   if(RA0<0):
      sign = -1
      ra   = -RA0
   else:
      sign = 1
      ra   = RA0

   h = int( ra/15. )
   ra -= h*15.
   m = int( ra*4.)
   ra -= m/4.
   s = ra*240.

   if(sign == -1):
      out = '-%02d:%02d:%06.3f'%(h,m,s)
   else: out = '+%02d:%02d:%06.3f'%(h,m,s)
   
   return out

def deg2DMS( DEC0 ):
    
   '''Borrowed from Sylvain Baumont 
    http://supernovae.in2p3.fr/~baumont/'''
    
   if(DEC0<0):
      sign = -1
      dec  = -DEC0
   else:
      sign = 1
      dec  = DEC0

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

