import File from 'fs/promises'
import Path from 'path'

const contents = new Map()

const files = (await File.readdir('./src', {recursive: true})).map(a => Path.resolve('./src', a)).filter(a => a.endsWith('.purs') || a.endsWith('.js'))
for (const f of files) {
  const fc = await File.readFile(f, 'utf8')
  const fc_ = fc.replaceAll(/\bTower\b/g, 'Axon')
  await File.writeFile(f, fc_)
  const f_ = f.replace(/\bTower\b/, 'Axon')
  await File.rename(f, f_)
  console.log(`${f} -> ${f_}`)
}
