const EventEmitter = require("./event-emitter");

let emitter;

beforeEach(() => {
  emitter = new EventEmitter();
});

describe("on", () => {
  test("should register and call a listener", () => {
    const fn = jest.fn();
    emitter.on("event", fn);

    emitter.emit("event", "arg1", "arg2");

    expect(fn).toHaveBeenCalledWith("arg1", "arg2");
    expect(fn).toHaveBeenCalledTimes(1);
  });

  test("should register multiple listeners for the same event", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.on("multi", fn1);
    emitter.on("multi", fn2);

    emitter.emit("multi", "data");

    expect(fn1).toHaveBeenCalledWith("data");
    expect(fn2).toHaveBeenCalledWith("data");
  });

  test("should register multiple listeners for the same event with chaining", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.on("multi", fn1).on("multi", fn2);

    emitter.emit("multi", "data");

    expect(fn1).toHaveBeenCalledWith("data");
    expect(fn2).toHaveBeenCalledWith("data");
  });
});

describe("emit", () => {
  test("should support chaining", () => {
    const fn = jest.fn();

    emitter.on("event", fn);

    emitter.emit("event", "data").emit("event", "data");

    expect(fn).toHaveBeenCalledTimes(2);
    expect(fn).toHaveBeenCalledWith("data");
  });

  test("should support chaining", () => {
    const fn = jest.fn();

    emitter.on("event", fn);

    emitter.emit("event", "data1").emit("event", "data2");

    expect(fn).toHaveBeenCalledTimes(2);
    expect(fn).toHaveBeenNthCalledWith(1, "data1");
    expect(fn).toHaveBeenNthCalledWith(2, "data2");
  });
});

test("should only call listener once", () => {
  const fn = jest.fn();
  emitter.once("onceEvent", fn);

  emitter.emit("onceEvent", 1);
  emitter.emit("onceEvent", 2);

  expect(fn).toHaveBeenCalledTimes(1);
  expect(fn).toHaveBeenCalledWith(1);
});

test("should remove a listener with off/removeListener", () => {
  const fn = jest.fn();

  emitter.on("remove", fn);
  emitter.off("remove", fn);

  emitter.emit("remove");

  expect(fn).not.toHaveBeenCalled();
});

test("should remove all listeners with removeAllListeners", () => {
  const fn1 = jest.fn();
  const fn2 = jest.fn();

  emitter.on("clear", fn1);
  emitter.on("clear", fn2);

  emitter.removeAllListeners("clear");
  emitter.emit("clear");

  expect(fn1).not.toHaveBeenCalled();
  expect(fn2).not.toHaveBeenCalled();
});

test("should return correct listeners with listeners()", () => {
  const fn = () => {};
  emitter.on("check", fn);

  const listeners = emitter.listeners("check");
  expect(listeners).toContain(fn);
  expect(listeners.length).toBe(1);
});

test("should not fail if removing unknown listener", () => {
  const fn = () => {};
  expect(() => emitter.off("nope", fn)).not.toThrow();
});

test("should pass multiple arguments to listener", () => {
  const fn = jest.fn();
  emitter.on("args", fn);

  emitter.emit("args", "a", "b", "c");

  expect(fn).toHaveBeenCalledWith("a", "b", "c");
});

test('should emit "error" event when no listener', () => {
  const error = new Error("boom");
  expect(() => emitter.emit("error", error)).toThrow(error);
});

test('should catch "error" event if listener exists', () => {
  const fn = jest.fn();
  const error = new Error("handled");

  emitter.on("error", fn);
  emitter.emit("error", error);

  expect(fn).toHaveBeenCalledWith(error);
});
