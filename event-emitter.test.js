const EventEmitter = require("./event-emitter");

let emitter;

beforeEach(() => {
  emitter = new EventEmitter();
});

describe("on", () => {
  it("should register and call a listener", () => {
    const fn = jest.fn();
    emitter.on("event", fn);
    emitter.emit("event", "arg1", "arg2");
    expect(fn).toHaveBeenCalledWith("arg1", "arg2");
    expect(fn).toHaveBeenCalledTimes(1);
  });
  it("should register multiple listeners for the same event", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();
    emitter.on("multi", fn1);
    emitter.on("multi", fn2);
    emitter.emit("multi", "data");
    expect(fn1).toHaveBeenCalledWith("data");
    expect(fn2).toHaveBeenCalledWith("data");
  });
  it("should register multiple listeners for the same event with chaining", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();
    emitter.on("multi", fn1).on("multi", fn2);
    emitter.emit("multi", "data");
    expect(fn1).toHaveBeenCalledWith("data");
    expect(fn2).toHaveBeenCalledWith("data");
  });
});
describe("emit", () => {
  it("should invoke listener in the order they are added", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();
    emitter.on("event", fn1);
    emitter.on("event", fn2);
    emitter.emit("event", "data");
    expect(fn1).toHaveBeenCalled();
    expect(fn2).toHaveBeenCalled();

    const callOrder1 = fn1.mock.invocationCallOrder[0];
    const callOrder2 = fn2.mock.invocationCallOrder[0];

    expect(callOrder1).toBeLessThan(callOrder2);
  });

  it("should support chaining", () => {
    const fn = jest.fn();
    emitter.on("event", fn);
    emitter.emit("event", "data").emit("event", "data");
    expect(fn).toHaveBeenCalledTimes(2);
    expect(fn).toHaveBeenCalledWith("data");
  });

  it("should support chaining", () => {
    const fn = jest.fn();
    emitter.on("event", fn);
    emitter.emit("event", "data1").emit("event", "data2");
    expect(fn).toHaveBeenCalledTimes(2);
    expect(fn).toHaveBeenNthCalledWith(1, "data1");
    expect(fn).toHaveBeenNthCalledWith(2, "data2");
  });
});
it("should only call listener once", () => {
  const fn = jest.fn();
  emitter.once("onceEvent", fn);
  emitter.emit("onceEvent", 1);
  emitter.emit("onceEvent", 2);
  expect(fn).toHaveBeenCalledTimes(1);
  expect(fn).toHaveBeenCalledWith(1);
});

describe("off/removeListener", () => {
  it("should remove a listener", () => {
    const fn = jest.fn();

    emitter.on("event", fn);
    emitter.removeListener("event", fn);

    emitter.emit("event");

    expect(fn).not.toHaveBeenCalled();
  });

  it("should not remove a listener if a function is passed not by reference", () => {
    const fn = jest.fn();

    emitter.on("event", fn);
    emitter.removeListener("event", jest.fn());

    emitter.emit("event");

    expect(fn).toHaveBeenCalled();
  });

  it("should not remove a listener if removeListener is called after emit", () => {
    const fn = jest.fn();

    emitter.on("event", fn);
    emitter.emit("event");

    emitter.removeListener("event", fn);
    expect(fn).toHaveBeenCalled();

    fn.mockReset();

    emitter.emit("event");
    expect(fn).not.toHaveBeenCalled();
  });

  it("should remove only one listener", () => {
    const fn = jest.fn();

    emitter.on("event", fn);
    emitter.on("event", fn);
    emitter.removeListener("event", fn);

    emitter.emit("event");

    expect(fn).toHaveBeenCalled();
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it("should remove two listeners", () => {
    const fn = jest.fn();

    emitter.on("event", fn);
    emitter.on("event", fn);
    emitter.removeListener("event", fn);
    emitter.removeListener("event", fn);

    emitter.emit("event");

    expect(fn).not.toHaveBeenCalled();
  });

  it("node.js example", () => {
    const callbackA = jest.fn().mockImplementation(() => {
      emitter.removeListener("event", callbackB);
    });
    const callbackB = jest.fn();

    emitter.on("event", callbackA);
    emitter.on("event", callbackB);

    // callbackA removes listener callbackB but it will still be called.
    // Internal listener array at time of emit [callbackA, callbackB]
    emitter.emit("event");

    expect(callbackA).toHaveBeenCalled();
    expect(callbackB).toHaveBeenCalled();

    callbackB.mockReset();

    // callbackB is now removed.
    // Internal listener array [callbackA]
    emitter.emit("event");

    expect(callbackA).toHaveBeenCalled();
    expect(callbackB).not.toHaveBeenCalled();
  });
});

it("should remove all listeners with removeAllListeners", () => {
  const fn1 = jest.fn();
  const fn2 = jest.fn();

  emitter.on("clear", fn1);
  emitter.on("clear", fn2);

  emitter.removeAllListeners("clear");
  emitter.emit("clear");

  expect(fn1).not.toHaveBeenCalled();
  expect(fn2).not.toHaveBeenCalled();
});

it("should return correct listeners with listeners()", () => {
  const fn = () => {};
  emitter.on("check", fn);

  const listeners = emitter.listeners("check");
  expect(listeners).toContain(fn);
  expect(listeners.length).toBe(1);
});

it("should not fail if removing unknown listener", () => {
  const fn = () => {};
  expect(() => emitter.off("nope", fn)).not.toThrow();
});

it("should pass multiple arguments to listener", () => {
  const fn = jest.fn();
  emitter.on("args", fn);

  emitter.emit("args", "a", "b", "c");

  expect(fn).toHaveBeenCalledWith("a", "b", "c");
});

it('should emit "error" event when no listener', () => {
  const error = new Error("boom");
  expect(() => emitter.emit("error", error)).toThrow(error);
});

it('should catch "error" event if listener exists', () => {
  const fn = jest.fn();
  const error = new Error("handled");

  emitter.on("error", fn);
  emitter.emit("error", error);

  expect(fn).toHaveBeenCalledWith(error);
});
