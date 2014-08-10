unit FileTransProtocol;

interface

const
  FILE_TRANS_FLAG = $A1;

type
  PFileHead = ^TFileHead;
  TFileHead = record
    Flag: Word;
    cmd: Word;  // 1, request file data, 2: response data, 10: reqeust fileInfo, 11:fileInfo response
    FileName: string[255];
    Position: Int64;
    Size: Int64;
    crc: Cardinal;
    cmd_result:Integer;  // 0:succ, 1:file not found, 2:exception, 3:error param
  end;


implementation

end.
