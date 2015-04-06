function gauss,t,p
  ;;p0=norm
  ;;p1=center
  ;;p2=width
  yfit=p[0]*exp(-(t-p[1])^2/(2*p[2]^2))
return,yfit
end

function intgauss,t,p
;  yfit=(gauss(t[0,*],p)+gauss(t[1,*],p))/2.
  t1=t[0,*]
  t2=t[1,*]
  yfit=sqrt(!pi/2.)*(-p[0])*p[2]*(erf((p[1]-t2)/(sqrt(2)*p[2]))-erf((p[1]-t1)/(sqrt(2)*p[2])))
  td=t2-t1
  yfit=yfit/td

return,yfit
end 

function gauss1_pow,t,p
  yfit=gauss(t,p[2:4])+pow(t,p[0:1])
return,yfit
end

function gauss2_pow,t,p
  yfit=gauss(t,p[2:4])+gauss(t,p[5:7])+pow(t,p[0:1])
return,yfit
end

function gauss3_pow,t,p
  yfit=gauss(t,p[2:4])+gauss(t,p[5:7])+gauss(t,p[8:10])+pow(t,p[0:1])
return,yfit
end

function gauss4_pow,t,p
  yfit=gauss(t,p[2:4])+gauss(t,p[5:7])+gauss(t,p[8:10])+gauss(t,p[11:13])+pow(t,p[0:1])
return,yfit
end

function gauss5_pow,t,p
  yfit=gauss(t,p[2:4])+gauss(t,p[5:7])+gauss(t,p[8:10])+gauss(t,p[11:13])+gauss(t,p[14:16])+pow(t,p[0:1])
return,yfit
end

function gauss6_pow,t,p
  yfit=gauss(t,p[2:4])+gauss(t,p[5:7])+gauss(t,p[8:10])+gauss(t,p[11:13])+gauss(t,p[14:16])+gauss(t,p[17:19])+pow(t,p[0:1])
return,yfit
end

function gauss7_pow,t,p
  yfit=gauss(t,p[2:4])+gauss(t,p[5:7])+gauss(t,p[8:10])+gauss(t,p[11:13])+gauss(t,p[14:16])+gauss(t,p[17:19])+gauss(t,p[20:22])+pow(t,p[0:1])
return,yfit
end

function gauss1_bknpow,t,p
  yfit=gauss(t,p[4:6])+bknpow(t,p[0:3])
return,yfit
end

function gauss2_bknpow,t,p
  yfit=gauss(t,p[4:6])+gauss(t,p[7:9])+bknpow(t,p[0:3])
return,yfit
end

function gauss3_bknpow,t,p
  yfit=gauss(t,p[4:6])+gauss(t,p[7:9])+gauss(t,p[10:12])+bknpow(t,p[0:3])
return,yfit
end

function gauss4_bknpow,t,p
  yfit=gauss(t,p[4:6])+gauss(t,p[7:9])+gauss(t,p[10:12])+gauss(t,p[13:15])+bknpow(t,p[0:3])
return,yfit
end

function gauss5_bknpow,t,p
  yfit=gauss(t,p[4:6])+gauss(t,p[7:9])+gauss(t,p[10:12])+gauss(t,p[13:15])+gauss(t,p[16:18])+bknpow(t,p[0:3])
return,yfit
end

function gauss6_bknpow,t,p
  yfit=gauss(t,p[4:6])+gauss(t,p[7:9])+gauss(t,p[10:12])+gauss(t,p[13:15])+gauss(t,p[16:18])+gauss(t,p[19:21])+bknpow(t,p[0:3])
return,yfit
end

function gauss7_bknpow,t,p
  yfit=gauss(t,p[4:6])+gauss(t,p[7:9])+gauss(t,p[10:12])+gauss(t,p[13:15])+gauss(t,p[16:18])+gauss(t,p[19:21])+gauss(t,p[22:24])+bknpow(t,p[0:3])
return,yfit
end

function gauss1_bkn2pow,t,p
  yfit=gauss(t,p[6:8])+bkn2pow(t,p[0:5])
return,yfit
end

function gauss2_bkn2pow,t,p
  yfit=gauss(t,p[6:8])+gauss(t,p[9:11])+bkn2pow(t,p[0:5])
return,yfit
end

function gauss3_bkn2pow,t,p
  yfit=gauss(t,p[6:8])+gauss(t,p[9:11])+gauss(t,p[12:14])+bkn2pow(t,p[0:5])
return,yfit
end

function gauss4_bkn2pow,t,p
  yfit=gauss(t,p[6:8])+gauss(t,p[9:11])+gauss(t,p[12:14])+gauss(t,p[15:17])+bkn2pow(t,p[0:5])
return,yfit
end

function gauss5_bkn2pow,t,p
  yfit=gauss(t,p[6:8])+gauss(t,p[9:11])+gauss(t,p[12:14])+gauss(t,p[15:17])+gauss(t,p[18:20])+bkn2pow(t,p[0:5])
return,yfit
end

function gauss6_bkn2pow,t,p
  yfit=gauss(t,p[6:8])+gauss(t,p[9:11])+gauss(t,p[12:14])+gauss(t,p[15:17])+gauss(t,p[18:20])+gauss(t,p[21:23])+bkn2pow(t,p[0:5])
return,yfit
end

function gauss7_bkn2pow,t,p
  yfit=gauss(t,p[6:8])+gauss(t,p[9:11])+gauss(t,p[12:14])+gauss(t,p[15:17])+gauss(t,p[18:20])+gauss(t,p[21:23])+gauss(t,p[24:26])+bkn2pow(t,p[0:5])
return,yfit
end

function gauss1_bkn3pow,t,p
  yfit=gauss(t,p[8:10])+bkn3pow(t,p[0:7])
return,yfit
end

function gauss2_bkn3pow,t,p
  yfit=gauss(t,p[8:10])+gauss(t,p[11:13])+bkn3pow(t,p[0:7])
return,yfit
end

function gauss3_bkn3pow,t,p
  yfit=gauss(t,p[8:10])+gauss(t,p[11:13])+gauss(t,p[14:16])+bkn3pow(t,p[0:7])
return,yfit
end

function gauss4_bkn3pow,t,p
  yfit=gauss(t,p[8:10])+gauss(t,p[11:13])+gauss(t,p[14:16])+gauss(t,p[17:19])+bkn3pow(t,p[0:7])
return,yfit
end

function gauss5_bkn3pow,t,p
  yfit=gauss(t,p[8:10])+gauss(t,p[11:13])+gauss(t,p[14:16])+gauss(t,p[17:19])+gauss(t,p[20:22])+bkn3pow(t,p[0:7])
return,yfit
end

function gauss6_bkn3pow,t,p
  yfit=gauss(t,p[8:10])+gauss(t,p[11:13])+gauss(t,p[14:16])+gauss(t,p[17:19])+gauss(t,p[20:22])+gauss(t,p[23:25])+bkn3pow(t,p[0:7])
return,yfit
end

function gauss7_bkn3pow,t,p
  yfit=gauss(t,p[8:10])+gauss(t,p[11:13])+gauss(t,p[14:16])+gauss(t,p[17:19])+gauss(t,p[20:22])+gauss(t,p[23:25])+gauss(t,p[26:28])+bkn3pow(t,p[0:7])
return,yfit
end

function gauss1_bkn4pow,t,p
  yfit=gauss(t,p[10:12])+bkn4pow(t,p[0:9])
return,yfit
end

function gauss2_bkn4pow,t,p
  yfit=gauss(t,p[10:12])+gauss(t,p[13:15])+bkn4pow(t,p[0:9])
return,yfit
end

function gauss3_bkn4pow,t,p
  yfit=gauss(t,p[10:12])+gauss(t,p[13:15])+gauss(t,p[16:18])+bkn4pow(t,p[0:9])
return,yfit
end

function gauss4_bkn4pow,t,p
  yfit=gauss(t,p[10:12])+gauss(t,p[13:15])+gauss(t,p[16:18])+gauss(t,p[19:21])+bkn4pow(t,p[0:9])
return,yfit
end

function gauss5_bkn4pow,t,p
  yfit=gauss(t,p[10:12])+gauss(t,p[13:15])+gauss(t,p[16:18])+gauss(t,p[19:21])+gauss(t,p[22:24])+bkn4pow(t,p[0:9])
return,yfit
end

function gauss6_bkn4pow,t,p
  yfit=gauss(t,p[10:12])+gauss(t,p[13:15])+gauss(t,p[16:18])+gauss(t,p[19:21])+gauss(t,p[22:24])+gauss(t,p[25:27])+bkn4pow(t,p[0:9])
return,yfit
end

function gauss7_bkn4pow,t,p
  yfit=gauss(t,p[10:12])+gauss(t,p[13:15])+gauss(t,p[16:18])+gauss(t,p[19:21])+gauss(t,p[22:24])+gauss(t,p[25:27])+gauss(t,p[28:30])+bkn4pow(t,p[0:9])
return,yfit
end

function intgauss1_pow,t,p
  yfit=intgauss(t,p[2:4])+intpow(t,p[0:1])
return,yfit
end

function intgauss2_pow,t,p
  yfit=intgauss(t,p[2:4])+intgauss(t,p[5:7])+intpow(t,p[0:1])
return,yfit
end

function intgauss3_pow,t,p
  yfit=intgauss(t,p[2:4])+intgauss(t,p[5:7])+intgauss(t,p[8:10])+intpow(t,p[0:1])
return,yfit
end

function intgauss4_pow,t,p
  yfit=intgauss(t,p[2:4])+intgauss(t,p[5:7])+intgauss(t,p[8:10])+intgauss(t,p[11:13])+intpow(t,p[0:1])
return,yfit
end

function intgauss5_pow,t,p
  yfit=intgauss(t,p[2:4])+intgauss(t,p[5:7])+intgauss(t,p[8:10])+intgauss(t,p[11:13])+intgauss(t,p[14:16])+intpow(t,p[0:1])
return,yfit
end

function intgauss6_pow,t,p
  yfit=intgauss(t,p[2:4])+intgauss(t,p[5:7])+intgauss(t,p[8:10])+intgauss(t,p[11:13])+intgauss(t,p[14:16])+intgauss(t,p[17:19])+intpow(t,p[0:1])
return,yfit
end

function intgauss7_pow,t,p
  yfit=intgauss(t,p[2:4])+intgauss(t,p[5:7])+intgauss(t,p[8:10])+intgauss(t,p[11:13])+intgauss(t,p[14:16])+intgauss(t,p[17:19])+intgauss(t,p[20:22])+intpow(t,p[0:1])
return,yfit
end

function intgauss1_bknpow,t,p
  yfit=intgauss(t,p[4:6])+intbknpow(t,p[0:3])
return,yfit
end

function intgauss2_bknpow,t,p
  yfit=intgauss(t,p[4:6])+intgauss(t,p[7:9])+intbknpow(t,p[0:3])
return,yfit
end

function intgauss3_bknpow,t,p
  yfit=intgauss(t,p[4:6])+intgauss(t,p[7:9])+intgauss(t,p[10:12])+intbknpow(t,p[0:3])
return,yfit
end

function intgauss4_bknpow,t,p
  yfit=intgauss(t,p[4:6])+intgauss(t,p[7:9])+intgauss(t,p[10:12])+intgauss(t,p[13:15])+intbknpow(t,p[0:3])
return,yfit
end

function intgauss5_bknpow,t,p
  yfit=intgauss(t,p[4:6])+intgauss(t,p[7:9])+intgauss(t,p[10:12])+intgauss(t,p[13:15])+intgauss(t,p[16:18])+intbknpow(t,p[0:3])
return,yfit
end

function intgauss6_bknpow,t,p
  yfit=intgauss(t,p[4:6])+intgauss(t,p[7:9])+intgauss(t,p[10:12])+intgauss(t,p[13:15])+intgauss(t,p[16:18])+intgauss(t,p[19:21])+intbknpow(t,p[0:3])
return,yfit
end

function intgauss7_bknpow,t,p
  yfit=intgauss(t,p[4:6])+intgauss(t,p[7:9])+intgauss(t,p[10:12])+intgauss(t,p[13:15])+intgauss(t,p[16:18])+intgauss(t,p[19:21])+intgauss(t,p[22:24])+intbknpow(t,p[0:3])
return,yfit
end

function intgauss1_bkn2pow,t,p
  yfit=intgauss(t,p[6:8])+intbkn2pow(t,p[0:5])
return,yfit
end

function intgauss2_bkn2pow,t,p
  yfit=intgauss(t,p[6:8])+intgauss(t,p[9:11])+intbkn2pow(t,p[0:5])
return,yfit
end

function intgauss3_bkn2pow,t,p
  yfit=intgauss(t,p[6:8])+intgauss(t,p[9:11])+intgauss(t,p[12:14])+intbkn2pow(t,p[0:5])
return,yfit
end

function intgauss4_bkn2pow,t,p
  yfit=intgauss(t,p[6:8])+intgauss(t,p[9:11])+intgauss(t,p[12:14])+intgauss(t,p[15:17])+intbkn2pow(t,p[0:5])
return,yfit
end

function intgauss5_bkn2pow,t,p
  yfit=intgauss(t,p[6:8])+intgauss(t,p[9:11])+intgauss(t,p[12:14])+intgauss(t,p[15:17])+intgauss(t,p[18:20])+intbkn2pow(t,p[0:5])
return,yfit
end

function intgauss6_bkn2pow,t,p
  yfit=intgauss(t,p[6:8])+intgauss(t,p[9:11])+intgauss(t,p[12:14])+intgauss(t,p[15:17])+intgauss(t,p[18:20])+intgauss(t,p[21:23])+intbkn2pow(t,p[0:5])
return,yfit
end

function intgauss7_bkn2pow,t,p
  yfit=intgauss(t,p[6:8])+intgauss(t,p[9:11])+intgauss(t,p[12:14])+intgauss(t,p[15:17])+intgauss(t,p[18:20])+intgauss(t,p[21:23])+intgauss(t,p[24:26])+intbkn2pow(t,p[0:5])
return,yfit
end

function intgauss1_bkn3pow,t,p
  yfit=intgauss(t,p[8:10])+intbkn3pow(t,p[0:7])
return,yfit
end

function intgauss2_bkn3pow,t,p
  yfit=intgauss(t,p[8:10])+intgauss(t,p[11:13])+intbkn3pow(t,p[0:7])
return,yfit
end

function intgauss3_bkn3pow,t,p
  yfit=intgauss(t,p[8:10])+intgauss(t,p[11:13])+intgauss(t,p[14:16])+intbkn3pow(t,p[0:7])
return,yfit
end

function intgauss4_bkn3pow,t,p
  yfit=intgauss(t,p[8:10])+intgauss(t,p[11:13])+intgauss(t,p[14:16])+intgauss(t,p[17:19])+intbkn3pow(t,p[0:7])
return,yfit
end

function intgauss5_bkn3pow,t,p
  yfit=intgauss(t,p[8:10])+intgauss(t,p[11:13])+intgauss(t,p[14:16])+intgauss(t,p[17:19])+intgauss(t,p[20:22])+intbkn3pow(t,p[0:7])
return,yfit
end

function intgauss6_bkn3pow,t,p
  yfit=intgauss(t,p[8:10])+intgauss(t,p[11:13])+intgauss(t,p[14:16])+intgauss(t,p[17:19])+intgauss(t,p[20:22])+intgauss(t,p[23:25])+intbkn3pow(t,p[0:7])
return,yfit
end

function intgauss7_bkn3pow,t,p
  yfit=intgauss(t,p[8:10])+intgauss(t,p[11:13])+intgauss(t,p[14:16])+intgauss(t,p[17:19])+intgauss(t,p[20:22])+intgauss(t,p[23:25])+intgauss(t,p[26:28])+intbkn3pow(t,p[0:7])
return,yfit
end

function intgauss1_bkn4pow,t,p
  yfit=intgauss(t,p[10:12])+intbkn4pow(t,p[0:9])
return,yfit
end

function intgauss2_bkn4pow,t,p
  yfit=intgauss(t,p[10:12])+intgauss(t,p[13:15])+intbkn4pow(t,p[0:9])
return,yfit
end

function intgauss3_bkn4pow,t,p
  yfit=intgauss(t,p[10:12])+intgauss(t,p[13:15])+intgauss(t,p[16:18])+intbkn4pow(t,p[0:9])
return,yfit
end

function intgauss4_bkn4pow,t,p
  yfit=intgauss(t,p[10:12])+intgauss(t,p[13:15])+intgauss(t,p[16:18])+intgauss(t,p[19:21])+intbkn4pow(t,p[0:9])
return,yfit
end

function intgauss5_bkn4pow,t,p
  yfit=intgauss(t,p[10:12])+intgauss(t,p[13:15])+intgauss(t,p[16:18])+intgauss(t,p[19:21])+intgauss(t,p[22:24])+intbkn4pow(t,p[0:9])
return,yfit
end

function intgauss6_bkn4pow,t,p
  yfit=intgauss(t,p[10:12])+intgauss(t,p[13:15])+intgauss(t,p[16:18])+intgauss(t,p[19:21])+intgauss(t,p[22:24])+intgauss(t,p[25:27])+intbkn4pow(t,p[0:9])
return,yfit
end

function intgauss7_bkn4pow,t,p
  yfit=intgauss(t,p[10:12])+intgauss(t,p[13:15])+intgauss(t,p[16:18])+intgauss(t,p[19:21])+intgauss(t,p[22:24])+intgauss(t,p[25:27])+intgauss(t,p[28:30])+intbkn4pow(t,p[0:9])
return,yfit
end
