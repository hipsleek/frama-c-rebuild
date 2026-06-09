// --------------------------------------------------------------------------
// --- Configure Packages
// --- Called by [make pkg]
// --------------------------------------------------------------------------

const path = require('path');
const fs = require('fs');

const loader = process.argv[2];
const inputFiles = process.argv.slice(3);
const packages = new Map();
const pluginsPath = "./src/renderer/public";
let buffer = '// Ivette Package Loader (generated)\n';

function error(message) {
  console.error(message);
  process.exit(1);
}

inputFiles.forEach((file) => {
  try {
    const pkgId = path.relative('./src',path.dirname(file));
    const pkgSrc = fs.readFileSync(file, { encoding: 'UTF-8' });
    const pkgJson = JSON.parse(pkgSrc);
    packages.set(pkgId,pkgJson);
  } catch(err) { error(`[Dome] Error ${file}: ${err}`); }
});

function depend(id) {
  const pkg = packages.get(id);
  if (pkg) configure(pkg,id);
}

// Merge source and target folder
function mergeDirectories(source, target) {
  if (!fs.existsSync(source)) error(`Source doesn't exist: ${source}`);

  fs.readdirSync(source).forEach(file => {
    const sourcePath = path.join(source, file);;
    const targetPath = path.join(target, file);

    if (fs.lstatSync(sourcePath).isDirectory()) {
      mergeDirectories(sourcePath, targetPath);
    } else {
      if (!fs.existsSync(targetPath)) {
        const pathFile = path.dirname(targetPath);
        if (!fs.existsSync(pathFile)) fs.mkdirSync(pathFile, { recursive: true });
        fs.copyFileSync(sourcePath, targetPath);
      } else error(`File already exist: ${targetPath}`);
    }
  });
}

// delete public folder
function delPublicFolder() {
  try { fs.rmSync(pluginsPath, { recursive: true, force: true }); }
  catch (err) { error(`Error deleting folder: ${err}`); }
}

function copyExtraResources(pkg, id) {
  if(!pkg.resources || !Array.isArray(pkg.resources)) return;
  pkg.resources.forEach((resource) => {
    if(resource)
      mergeDirectories(
        path.join('./src', id, resource),
        path.join(pluginsPath, id, resource)
      );
  })
}

function configure(pkg, id) {
  if (!pkg.done) {
    pkg.done = true;
    /** add plugins extra resources in public folder */
    copyExtraResources(pkg, id);
    for(let parent = id;;) {
      parent = path.dirname(parent);
      if (!parent || parent === '.') break;
      depend(parent);
    }
    const { depends=[], main='.' } = pkg;
    depends.forEach(depend);
    console.log(`[Ivette] package ${id}`);
    buffer += `import '${path.join(id,main)}';\n`;
  }
}

delPublicFolder(); // Delete public folder
mergeDirectories('./src/renderer/resources',  pluginsPath); // merge permanent resources

packages.forEach(configure);
fs.writeFileSync(loader, buffer);
