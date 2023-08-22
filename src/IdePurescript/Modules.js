import * as os from "os"

export function tmpDir() {
  return os.tmpdir();
}
