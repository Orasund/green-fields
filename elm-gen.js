"use strict";

const Handlebars = require("handlebars")
const fs = require("fs")

const ELM_GEN = "elm-gen"

try { 
    const data = JSON.parse(fs.readFileSync(ELM_GEN + ".json", "utf8"))
    const templateData = fs.readFileSync("." + ELM_GEN + "/templates/" + data.template + ".elm", "utf8")
    const template = Handlebars.compile(templateData)
    const output = template(data)
    const dir = ("." + ELM_GEN + "/generated/") + data.moduleBase.replace(".","/") + ("/" + data.template)
    const generatedPath =  dir + "/" + data.typeName  + ".elm"

    //create folder structure
    if (!fs.existsSync(dir)) fs.mkdirSync(dir, { recursive: true });
    
    fs.writeFileSync(generatedPath, output, {flag : "w+"})

} catch (err) {
    console.error(err)
}

