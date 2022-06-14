import { writeFileSync } from 'fs';

export function whichSyncImpl (options) {
  var which = require('which');
  return function (path) {
    return function() {
      return which.sync(path, { all: true, path: options.path, pathExt: options.pathExt });
    };
  };
}
