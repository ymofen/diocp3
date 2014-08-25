unit uStreamCoderSocket;

interface

uses
  uICoderSocket, Classes, SysUtils;

type
  {$if CompilerVersion < 18}
    TBytes = array of Byte;
  {$IFEND}

  TStreamCoderSocket = class(TObject)
  private
    class function SendStream(pvSocket: ICoderSocket; pvStream: TStream): Integer;
  public
    /// <summary>
    ///   接收解码
    /// </summary>
    /// <returns> Boolean
    /// </returns>
    /// <param name="pvSocket"> (TClientSocket) </param>
    /// <param name="pvObject"> (TObject) </param>
    class function RecvObject(pvSocket: ICoderSocket; pvObject: TObject): Boolean;

    /// <summary>
    ///   编码发送
    /// </summary>
    /// <param name="pvSocket"> (TClientSocket) </param>
    /// <param name="pvDataObject"> (TObject) </param>
    class function SendObject(pvSocket: ICoderSocket; pvObject: TObject): Integer;
  end;


implementation

uses
  uByteTools;

const
  PACK_FLAG = $D10;
  MAX_OBJECT_SIZE = 1024 * 1024 * 10;  //最大对象大小 10M , 大于10M 则会认为错误的包。

const
  BUF_BLOCK_SIZE = 1024 * 8;



resourcestring
  strRecvException_ErrorFlag = '错误的包头数据,断开与服务器的连接';
  strRecvException_ErrorData = '错误的数据,断开与服务器的连接';
  strSendException_TooBig = '数据包太大,请在业务层分拆发送,最大数据包[%d]!';
  strSendException_NotEqual = '发送Buffer错误指定发送%d,实际发送:%d';





{ TStreamCoderSocket }

class function TStreamCoderSocket.RecvObject(pvSocket: ICoderSocket;
  pvObject: TObject): Boolean;
var
  lvBytes:TBytes;
  lvReadL:Integer;
  lvPACK_FLAG:Word;
  lvDataLen: Integer;
begin
  pvSocket.recvBuf(@lvPACK_FLAG, 2);

  if lvPACK_FLAG <> PACK_FLAG then
  begin
    pvSocket.closeSocket;
    //错误的包数据
    raise Exception.Create(strRecvException_ErrorFlag);
  end;

  //headlen
  pvSocket.recvBuf(@lvReadL, SizeOf(lvReadL));
  lvDataLen := TByteTools.swap32(lvReadL);

  //文件头不能过大
  if lvDataLen > MAX_OBJECT_SIZE  then
  begin
    //错误的包数据
    pvSocket.closeSocket;
    //错误的包数据
    raise Exception.Create(strRecvException_ErrorData);
  end;

  SetLength(lvBytes,lvDataLen);
  pvSocket.recvBuf(@lvBytes[0], lvDataLen);

  TStream(pvObject).Write(lvBytes[0], lvDataLen);
  Result := true;                                
end;

class function TStreamCoderSocket.SendObject(pvSocket: ICoderSocket; pvObject:
    TObject): Integer;
var
  lvPACK_FLAG: WORD;
  lvDataLen, lvWriteIntValue: Integer;
  lvBuf: TBytes;
  lvStream:TMemoryStream;
begin
  lvPACK_FLAG := PACK_FLAG;

  lvStream := TMemoryStream.Create;
  try
    TStream(pvObject).Position := 0;

    if TStream(pvObject).Size > MAX_OBJECT_SIZE then
    begin
       raise Exception.CreateFmt(strSendException_TooBig, [MAX_OBJECT_SIZE]);
    end;

    //pack_flag
    lvStream.Write(lvPACK_FLAG, 2);

    //
    lvDataLen := TStream(pvObject).Size;
    lvWriteIntValue := TByteTools.swap32(lvDataLen);
    
    // stream len
    lvStream.Write(lvWriteIntValue, SizeOf(lvWriteIntValue));

    // send pack
    lvStream.CopyFrom(TStream(pvObject), lvDataLen);

    Result := SendStream(pvSocket, lvStream);
  finally
    lvStream.Free;
  end;
end;

class function TStreamCoderSocket.SendStream(pvSocket: ICoderSocket; pvStream:
    TStream): Integer;
var
  lvBufBytes:array[0..BUF_BLOCK_SIZE-1] of byte;
  l, j, lvTotal:Integer;
begin
  Result := 0;
  if pvStream = nil then Exit;
  if pvStream.Size = 0 then Exit;
  lvTotal :=0;
  
  pvStream.Position := 0;
  repeat
    //FillMemory(@lvBufBytes[0], SizeOf(lvBufBytes), 0);
    l := pvStream.Read(lvBufBytes[0], SizeOf(lvBufBytes));
    if (l > 0) then
    begin
      j:=pvSocket.sendBuf(@lvBufBytes[0], l);
      if j <> l then
      begin
        raise Exception.CreateFmt(strSendException_NotEqual, [j, l]);
      end else
      begin
        lvTotal := lvTotal + j;
      end;
    end else Break;
  until (l = 0);
  Result := lvTotal;
end;

end.
