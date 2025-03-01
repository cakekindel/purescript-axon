/** @type {(s: import('node:net').SocketAddress) => string} */
export const nodeAddr = s => s.address

/** @type {(s: import('node:net').SocketAddress) => number} */
export const nodePort = s => s.port
