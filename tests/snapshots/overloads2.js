// @filename: http.ts


// @filename: compute.ts
import { HttpRoute, PathArgs, RequestHandler, RequestHandlerWithBody, PathArgsWithBody } from "./http"

// @filename: main.ts
import { HttpService } from './compute'
const s = new HttpService()
export const hello = s.route('GET', '/{foo}/{bar}/{fizz}', () => {
  return 'hello, world!'
})
