"use strict";

const Handlebars = require("handlebars")
const fs = require("fs")

const ELM_GEN = "elm-gen"

try { 
    //Read files
    const data = JSON.parse(fs.readFileSync(ELM_GEN + ".json", "utf8"))
    const templateData = fs.readFileSync("." + ELM_GEN + "/templates/" + data.template + ".elm", "utf8")
    
    //Register additional functions
    Handlebars.registerHelper('capitalize', function (aString) {
        return aString[0].toUpperCase() + aString.substring(1);
    })
    Handlebars.registerHelper('decapitalize', function (aString) {
        return aString[0].toLowerCase() + aString.substring(1);
    })

    //compile files
    const template = Handlebars.compile(templateData,{strict : true})
    const output = template(data)
    const dir = ("." + ELM_GEN + "/generated/") + data.moduleBase.replace(".","/") + ("/" + data.template)
    const generatedPath =  dir + "/" + data.typeName  + ".elm"

    //create folder structure
    if (!fs.existsSync(dir)) fs.mkdirSync(dir, { recursive: true });
    
    //create file
    fs.writeFileSync(generatedPath, output, {flag : "w+"})

} catch (err) {
    console.error(err)
}

