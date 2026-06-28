package vutils

package object buf {
  type BufBackend = vutils.buf.BufStorageBackend

  val RegBufBackend: vutils.buf.BufStorageBackend     = vutils.buf.BufStorageBackend.Reg
  val MemBufBackend: vutils.buf.BufStorageBackend     = vutils.buf.BufStorageBackend.Mem
  val SyncMemBufBackend: vutils.buf.BufStorageBackend = vutils.buf.BufStorageBackend.SyncMem
}
