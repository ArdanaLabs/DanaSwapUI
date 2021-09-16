import 'jest-extended';
declare global {
  namespace jest {
    interface Matchers<R> {
      nullOrAny(expected: R): CustomMatcherResult;
    }
  }
}