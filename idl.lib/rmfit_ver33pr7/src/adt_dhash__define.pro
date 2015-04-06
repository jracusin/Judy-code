FUNCTION adt_dhash::init, tbl_size, LOADFACTOR=load

	IF N_ELEMENTS(tbl_size) EQ 0 THEN self.size = 100L $
	ELSE self.size = tbl_size
	
	IF N_ELEMENTS(load) EQ 0 THEN load = .55
	
	self.maxLoad = LONG(load * self.size)
	
	table = PTRARR(self.size)
	self.handle = PTR_NEW(table)
	
	self.keys = PTR_NEW(STRARR(self.size))
	
	RETURN,1

END

PRO adt_dhash::set,key,value

	ON_ERROR,2

	i = self->hashf(key)
	
	;print,'map{'+key+'}=',i
	
	IF i EQ -1 THEN RETURN
	
	IF PTR_VALID((*self.handle)[i]) THEN BEGIN
		*((*self.handle)[i])[0] = value
		RETURN
	ENDIF
	
	(*self.handle)[i] = PTR_NEW(value)
	(*self.keys)[i] = key
END

PRO adt_dhash::remove,key

	ON_ERROR,2

	i = self->hashf(key)
	IF i EQ -1 THEN RETURN	
	IF NOT PTR_VALID((*self.handle)[i]) THEN BEGIN
		(*self.keys)[i] = ''
		RETURN
	ENDIF
	
	PTR_FREE, (*self.handle)[i]
	(*self.keys)[i] = ''
END

FUNCTION adt_dhash::get,key
	i = self->hashf(key)
	node = (*self.handle)[i]
	IF NOT PTR_VALID(node) THEN RETURN,PTR_NEW()
	IF ((*self.keys)[i] NE key) THEN RETURN, PTR_NEW()
	
	RETURN, *((*self.handle)[i])[0]
END

FUNCTION adt_dhash::keys, count
	inds = WHERE(*self.keys NE '', count)
	IF count GT 0 THEN RETURN, (*self.keys)[inds] $ 
	ELSE RETURN,-1
END

FUNCTION adt_dhash::dhash_incr, key
	i = self->base36(key)
	RETURN, (i^2 + (i-1)^2 ) MOD self.size
	RETURN, 1
END

FUNCTION adt_dhash::hashf, key
	IF SIZE(key, /TYPE) NE 7 THEN key = STRTRIM(STRING(key),2)

	i = self->base36(key)
	j = 0L
	WHILE (PTR_VALID((*self.handle)[i])) AND ((*self.keys)[i] NE key) DO BEGIN
		;print,key, i
		i = (i + self->dhash_incr(key) ) MOD self.size
		IF j EQ self.maxLoad THEN BEGIN
			print,"ADT_DHASH::HASHF(): Unresolved collision with key ",key, " at ", i
			RETURN,-1
		ENDIF
		j++
	ENDWHILE

	RETURN,i
END

FUNCTION adt_dhash::base36, key	
	index = ULONG(0)
	bkey = BYTE(key)

	FOR i=0L,strlen(key)-1 DO BEGIN
		index += ULONG( ( bkey[i] - BYTE('0') + 1 )* (36L^(strlen(key)- i - 1)) )
	ENDFOR
	index = index MOD self.size
	RETURN,index
END

PRO adt_dhash::Cleanup
	HEAP_FREE,self.handle
	PTR_FREE,self.keys
END

PRO adt_dhash__define

	struct = {ADT_DHASH, $
		size:0L,$
		maxLoad:0L,$
		handle:PTR_NEW(), $
		keys:PTR_NEW() $
		}
	

END