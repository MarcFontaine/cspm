print ("initializing lua embedding")
local cspmPrelude = assert(loadstring(_cspm_cspmPrelude()))
CSPM=cspmPrelude()
