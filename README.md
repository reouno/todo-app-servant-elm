# todo-app-servant-elm

# Environment

- Haskell Stack v1.9.3
- node v6.13.1
- yarn v1.13.0
- Elm 0.19.0 (automatically installed by `yarn install`)

# Run

### build all and run

```
./build_run.sh
```

### execute commands step by step

```
# build backend
stack build

# build frontend
cd frontend
yarn install # only for the first time
yarn webpack

# run server
cd ..
stack exec todo-app-servant-elm-exe
```

access to [http://localhost:3000](http://localhost:3000)

# Todo

- [x] create new todo
- [x] check done to todo
- [x] delete todo
- [x] show todos not done yet
- [x] show todos already done
- [x] automatically record done datetime
- [ ] auto deletion of old and done todos
- [ ] set due to todo
- [ ] reminder feature
- [ ] user account registration
- [ ] user authentication
- [ ] activity statistics
- [ ] todo contents statistics
- [ ] sofisticated UI and beautiful design

## License

This software is released under the MIT license, see [LICENSE](https://github.com/reouno/todo-app-servant-elm/blob/master/LICENSE).
