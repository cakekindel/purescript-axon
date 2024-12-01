import Fs from 'fs/promises'
import Path from 'path'

export const rootDir = Path.resolve(__dirname, '..')

export const packageDirs = async () => ['./src']

export const packageSources = async () => {
  const packages = await packageDirs()
  const sources = []
  for (const p of packages) {
    const files = await Fs.readdir(p, { recursive: true, withFileTypes: true })
    sources.push(
      ...files.flatMap(e =>
        e.isFile() ? [Path.resolve(rootDir, e.path, e.name)] : [],
      ),
    )
  }
  return sources
}
