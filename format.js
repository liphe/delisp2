const fs = require("fs");
const prettier = require("prettier");

const input = process.argv[2]
const output = process.argv[3]

function readASTFromFile(file) {
  const content = fs.readFileSync(file, 'utf-8')
  return JSON.parse(content)
}

const ast = readASTFromFile(input)

const js = prettier.format(
  ".", // any non-empty, non-whitespace string
  { parser: () => ast }
)

if (output === '-') {
  console.log(js)
} else {
  fs.writeFileSync(output, js)
}
