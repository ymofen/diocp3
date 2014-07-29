(*	
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *	 v0.1(2014-7-16 21:36:30)
 *     + first release
 *) 
unit iocpProtocol;

interface

uses
  Windows, SysUtils;


{$if CompilerVersion <= 23}
type
     NativeUInt = Cardinal;
     IntPtr = Cardinal;
{$ifend}



const
  IOCP_RESULT_OK = 0;
  IOCP_RESULT_QUIT = 1;



implementation

end.
