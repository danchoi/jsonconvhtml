# jsonextfilter

Applies external unix filters to specified key paths of a JSON object stream.

## Example

example.json:

```json
{
  "title": "Terminator 2: Judgment Day",
  "year": 1991,
  "stars": [
    {"name": "Arnold Schwarzenegger"},
    {"name": "Linda Hamilton"}
  ],
  "ratings": {
    "imdb": 8.5
  },
  "description":"<p>Some <strong>HTML</strong></p>"

}
{
  "title": "Interstellar",
  "year": 2014,
  "stars": [
    {"name":"Matthew McConaughey"},
    {"name":"Anne Hathaway"}
  ],
  "ratings": {
    "imdb": 8.9
  },
  "description":"<p>Some <strong>more HTML</strong></p>"
}
```

We want to transform the "description" fields from HTML to plain text:

```bash
jsonbf 'elinks -dump'  'description' < example.json  | jq -M '.' 
```

Output:

```json
{
  "ratings": {
    "imdb": 8.5
  },
  "stars": [
    {
      "name": "Arnold Schwarzenegger"
    },
    {
      "name": "Linda Hamilton"
    }
  ],
  "year": 1991,
  "title": "Terminator 2: Judgment Day",
  "description": "   Some HTML\n"
}
{
  "ratings": {
    "imdb": 8.9
  },
  "stars": [
    {
      "name": "Matthew McConaughey"
    },
    {
      "name": "Anne Hathaway"
    }
  ],
  "year": 2014,
  "title": "Interstellar",
  "description": "   Some more HTML\n"
}
```

More than one keypath can be specified in the keypaths argument string. Separate keypaths with spaces. The bash filter will be applied to all of them, e.g.

```bash
jsonextfilter 'elinks -dump'  'description review' < example.json  | jq -M '.' 
```

Currently only ONE bash filter command can be given. If you want to apply a pipeline of commands, wrap it in a bash script.
