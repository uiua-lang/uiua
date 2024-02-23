import { readFileSync } from "fs";
let md = readFileSync("changelog.md", "utf-8")
if (md.includes(`## ${process.argv[2]}`)) {
    let notes = md
        .replaceAll(process.argv[2], "\0VER\0")
        .match(/(?<=^|\n)## \0VER\0 .*?(?=\n## |$)/s)[0]
        .replaceAll("\0VER\0", process.argv[2])
        .trim()
    console.log(notes)
}
