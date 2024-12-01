/// <reference lib="dom" />
/// <reference lib="dom.iterable" />

/** @type {(_: {tuple: <A, B>(a: A) => (b: B) => unknown}) => (h: Headers) => () => Array<unknown>} */
export const headerEntries = ({tuple}) => hs => () => Array.from(hs.entries()).map(([a, b]) => tuple(a)(b))
