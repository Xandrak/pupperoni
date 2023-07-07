# Welcome
![Elm Tooling](https://github.com/Xandrak/pupperoni/actions/workflows/elm-tooling.yml/badge.svg)

Small, example Elm application utilizing a dog breed API.

## How to Run
If you have [elm-live](https://www.elm-live.com/) use the following command from the repo directory:
```
elm-live src/Main.elm -- --optimize --output=app.js 
```
If you'd like debug capabilities use the following:
```
elm-live src/Main.elm -- --output=app.js --debug
```

If you do not have `elm-live` you can use `elm reactor`:
```
elm reactor
```

Regardless of either approach, you will need to navigate to `http://localhost:8000`.

If using `elm-live`, you should see the home page.
If using `elm reactor` you will need to click on `src/Main.elm`.

## Future Enhancements
1. Accessibility - style the app utilizing WCAG 
2. Add ID's to appropriate elements -- can make testing easier with Browserstack/LogRocket/etc and also helps with Accessibility
3. STYLING -- needs pizzazz
4. Better error handling and communication to user.
5. Display the breed selected to user on images page.
6. Maybe refactor using query params for pagination.
7. Better UI - Scroll back to top when clicking Next/Previous buttons.
