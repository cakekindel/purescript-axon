import { $ } from 'bun'
import { packageSources } from './common.js'

const check = process.argv.includes('--check')

const sources = await packageSources()
const purs = sources.filter(f => f.endsWith('.purs'))
const js = sources
  .filter(f => f.endsWith('.js'))
  .concat(['./scripts/**/*.js', '.prettierrc.cjs'])
const json = ['package.json', 'jsconfig.json']
const yml = sources.filter(f => f.endsWith('.yaml')).concat(['spago.yaml'])

/** @type {(parser: string, ps: string[]) => import("bun").ShellPromise} */
const prettier = (parser, ps) =>
  $`bun x prettier ${check ? '--check' : '--write'} '--parser' ${parser} ${ps}`

const procs = [
  () => prettier('babel', js),
  () => prettier('json', json),
  () => prettier('yaml', yml),
  () =>
    prettier(
      'markdown',
      sources.filter(f => f.endsWith('.md')).concat(['README.md']),
    ),
  () => $`bun x purs-tidy ${check ? 'check' : 'format-in-place'} ${purs}`,
]
  .map(go => async () => {
    const p = await go().nothrow().quiet()
    if (p.exitCode === 0) return
    process.stdout.write(p.stdout)
    process.stderr.write(p.stderr)
    process.exit(1)
  })
  .reduce((acc, go) => acc.then(() => go()), Promise.resolve())

await procs
