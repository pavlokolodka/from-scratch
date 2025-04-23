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

    emitter.on("event", fn1);
    emitter.on("event", fn2);

    emitter.emit("event", "data");

    expect(fn1).toHaveBeenCalledWith("data");
    expect(fn2).toHaveBeenCalledWith("data");
  });

  it("should register multiple listeners for the same event with chaining", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.on("event", fn1).on("event", fn2);
    emitter.emit("event", "data");

    expect(fn1).toHaveBeenCalledWith("data");
    expect(fn2).toHaveBeenCalledWith("data");
  });
});

describe("once", () => {
  it("should register and call a listener", () => {
    const fn = jest.fn();

    emitter.once("event", fn);

    emitter.emit("event", "arg1", "arg2");

    expect(fn).toHaveBeenCalledWith("arg1", "arg2");
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it("should register multiple listeners for the same event", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.once("event", fn1);
    emitter.once("event", fn2);

    emitter.emit("event", "data");

    expect(fn1).toHaveBeenCalledWith("data");
    expect(fn2).toHaveBeenCalledWith("data");
  });

  it("should only call listener once", () => {
    const fn = jest.fn();

    emitter.once("event", fn);

    emitter.emit("event", 1);
    emitter.emit("event", 2);

    expect(fn).toHaveBeenCalledTimes(1);
    expect(fn).toHaveBeenCalledWith(1);
  });

  it("should register multiple listeners for the same event with chaining", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.once("event", fn1).once("event", fn2);
    emitter.emit("event", "data");

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

  it("should pass multiple arguments to listener", () => {
    const fn = jest.fn();
    emitter.on("args", fn);

    emitter.emit("args", "a", "b", "c");

    expect(fn).toHaveBeenCalledWith("a", "b", "c");
  });

  it("should support chaining (same arguments)", () => {
    const fn = jest.fn();

    emitter.on("event", fn);

    emitter.emit("event", "data").emit("event", "data");

    expect(fn).toHaveBeenCalledTimes(2);
    expect(fn).toHaveBeenCalledWith("data");
  });

  it("should support chaining (different arguments)", () => {
    const fn = jest.fn();

    emitter.on("event", fn);

    emitter.emit("event", "data1").emit("event", "data2");
    expect(fn).toHaveBeenCalledTimes(2);
    expect(fn).toHaveBeenNthCalledWith(1, "data1");
    expect(fn).toHaveBeenNthCalledWith(2, "data2");
  });
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

  it("should not fail if removing unknown listener", () => {
    const fn = () => {};
    expect(() => emitter.off("nope", fn)).not.toThrow();
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
      emitter.on("event", callbackC);
    });
    const callbackB = jest.fn();
    const callbackC = jest.fn();

    emitter.on("event", callbackA);
    emitter.on("event", callbackB);

    // callbackA removes listener callbackB but it will still be called.
    // Internal listener array at time of emit [callbackA, callbackB]
    emitter.emit("event");

    expect(callbackA).toHaveBeenCalled();
    expect(callbackB).toHaveBeenCalled();
    expect(callbackC).not.toHaveBeenCalled();

    callbackB.mockReset();
    // callbackB is now removed.
    // Internal listener array [callbackA, callbackC]
    emitter.emit("event");

    expect(callbackA).toHaveBeenCalled();
    expect(callbackB).not.toHaveBeenCalled();
    expect(callbackC).toHaveBeenCalled();
  });
});

describe("removeAllListeners", () => {
  it("should remove all listeners if event is not specified", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.on("event", fn1);
    emitter.on("event2", fn2);

    emitter.removeAllListeners();
    emitter.emit("event");
    emitter.emit("event2");

    expect(fn1).not.toHaveBeenCalled();
    expect(fn2).not.toHaveBeenCalled();
  });

  it("should remove all listeners by event", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();
    const fn3 = jest.fn();

    emitter.on("event", fn1);
    emitter.on("event", fn2);
    emitter.on("event2", fn3);

    emitter.removeAllListeners("event");
    emitter.emit("event");
    emitter.emit("event2");

    expect(fn1).not.toHaveBeenCalled();
    expect(fn2).not.toHaveBeenCalled();
    expect(fn3).toHaveBeenCalled();
  });

  it("should not remove a listener if removeAllListeners is called after emit", () => {
    const fn = jest.fn();

    emitter.on("event", fn);
    emitter.emit("event");

    emitter.removeAllListeners("event");
    expect(fn).toHaveBeenCalled();

    fn.mockReset();

    emitter.emit("event");
    expect(fn).not.toHaveBeenCalled();
  });
});

describe("listenerCount", () => {
  it("should return number of listeners for event when there is no listeners attached", () => {
    const result = emitter.listenerCount("event");

    expect(result).toBe(0);
  });

  it("should return number of listeners for event", () => {
    const fn = jest.fn();

    emitter.on("event", fn);

    const result = emitter.listenerCount("event");

    expect(result).toBe(1);
  });

  it("should return number of listeners for event for a particular listener", () => {
    const fn = jest.fn();
    const fn2 = jest.fn();

    emitter.on("event", fn);
    emitter.on("event", fn2);

    const result = emitter.listenerCount("event");
    const resultForListener = emitter.listenerCount("event", fn);

    expect(result).toBe(2);
    expect(resultForListener).toBe(1);
  });
});

describe("eventNames", () => {
  it("should return an array of events", () => {
    const fn = jest.fn();
    const events = ["event1", "event2", "event3"];

    for (const event of events) {
      emitter.on(event, fn);
    }

    const registeredEvents = emitter.eventNames();

    expect(registeredEvents).toEqual(events);
  });

  it("should return an array of events after unregistering an event", () => {
    const fn = jest.fn();

    const events = ["event1", "event2", "event3"];

    for (const event of events) {
      emitter.on(event, fn);
    }

    emitter.removeListener("event3", fn);
    events.pop();

    const registeredEvents = emitter.eventNames();

    expect(registeredEvents).toEqual(events);
  });

  it("should return an empty array of events", () => {
    const registeredEvents = emitter.eventNames();

    expect(registeredEvents).toEqual([]);
  });

  it("should return an array with undefined", () => {
    const fn = jest.fn();

    emitter.on(undefined, fn);

    const registeredEvents = emitter.eventNames();

    expect(registeredEvents).toEqual(["undefined"]);
  });
});

describe("getMaxListeners", () => {
  it("should return the default max listeners", () => {
    expect(emitter.getMaxListeners()).toBe(10);
  });

  it("should return the updated max listeners after setMaxListeners is called", () => {
    emitter.setMaxListeners(25);
    expect(emitter.getMaxListeners()).toBe(25);
  });
});

describe("setMaxListeners", () => {
  it("should update max listeners when passed a valid number", () => {
    emitter.setMaxListeners(5);
    expect(emitter.getMaxListeners()).toBe(5);
  });

  it("should throw an error if the argument is not a number", () => {
    expect(() => emitter.setMaxListeners("not a number")).toThrowError(
      'The "setMaxListeners" argument must be of type number. Received not a number'
    );
  });

  describe("invalid types", () => {
    it.each([
      ["a string", "string", "string"],
      ["null", null, "null"],
      ["undefined", undefined, "undefined"],
      ["a boolean (true)", true, "true"],
      ["a boolean (false)", false, "false"],
      ["an object", {}, "[object Object]"],
      ["an array", [], ""],
      ["a function", () => {}, "() => {}"],
      ["a symbol", Symbol("sym"), "Symbol(sym)"],
      ["a bigint", 10n, "10"],
    ])(
      "should throw an error when value is %s",
      (_label, input, expectedString) => {
        expect(() => emitter.setMaxListeners(input)).toThrowError(
          new Error(
            `The "setMaxListeners" argument must be of type number. Received ${expectedString}`
          )
        );
      }
    );
  });
});

describe("prependListener", () => {
  it("should add a listener to the beginning of the list (on)", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.on("event", fn1);
    emitter.prependListener("event", fn2);

    emitter.emit("event");
    const callOrder1 = fn1.mock.invocationCallOrder[0];
    const callOrder2 = fn2.mock.invocationCallOrder[0];

    expect(callOrder1).toBeGreaterThan(callOrder2);

    fn1.mockReset();
    fn2.mockReset();
    emitter.emit("event");

    expect(fn1).toHaveBeenCalled();
    expect(fn2).toHaveBeenCalled();
  });

  it("should add a listener to the beginning of the list (once)", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.once("event", fn1);
    emitter.prependListener("event", fn2);

    emitter.emit("event");
    const callOrder1 = fn1.mock.invocationCallOrder[0];
    const callOrder2 = fn2.mock.invocationCallOrder[0];

    expect(callOrder1).toBeGreaterThan(callOrder2);

    fn1.mockReset();
    fn2.mockReset();
    emitter.emit("event");

    expect(fn1).not.toHaveBeenCalled();
    expect(fn2).toHaveBeenCalled();
  });

  it("should add a listener to the beginning of the list (prependListener)", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.prependListener("event", fn1);
    emitter.prependListener("event", fn2);

    emitter.emit("event");
    const callOrder1 = fn1.mock.invocationCallOrder[0];
    const callOrder2 = fn2.mock.invocationCallOrder[0];

    expect(callOrder1).toBeGreaterThan(callOrder2);

    fn1.mockReset();
    fn2.mockReset();
    emitter.emit("event");

    expect(fn1).toHaveBeenCalled();
    expect(fn2).toHaveBeenCalled();
  });

  it("should add a listener to the beginning of the list (prependOnceListener)", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.prependOnceListener("event", fn1);
    emitter.prependListener("event", fn2);

    emitter.emit("event");
    const callOrder1 = fn1.mock.invocationCallOrder[0];
    const callOrder2 = fn2.mock.invocationCallOrder[0];

    expect(callOrder1).toBeGreaterThan(callOrder2);

    fn1.mockReset();
    fn2.mockReset();
    emitter.emit("event");

    expect(fn1).not.toHaveBeenCalled();
    expect(fn2).toHaveBeenCalled();
  });

  it("should add a listener to the emply list", () => {
    const fn = jest.fn();
    emitter.prependListener("event", fn);

    emitter.emit("event");
    expect(fn).toHaveBeenCalled();

    fn.mockReset();
    emitter.emit("event");

    expect(fn).toHaveBeenCalled();
  });

  it("should throw if listener is not a function", () => {
    expect(() => emitter.prependListener("event", "string")).toThrow(
      "Listener should be a function, received string"
    );
  });
});

describe("prependOnceListener", () => {
  it("should add and remove a listener to the beginning of the list (on)", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.on("event", fn1);
    emitter.prependOnceListener("event", fn2);

    emitter.emit("event");
    const callOrder1 = fn1.mock.invocationCallOrder[0];
    const callOrder2 = fn2.mock.invocationCallOrder[0];

    expect(callOrder1).toBeGreaterThan(callOrder2);

    fn1.mockReset();
    fn2.mockReset();
    emitter.emit("event");

    expect(fn1).toHaveBeenCalled();
    expect(fn2).not.toHaveBeenCalled();
  });

  it("should add and remove a listener to the beginning of the list (once)", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.once("event", fn1);
    emitter.prependOnceListener("event", fn2);

    emitter.emit("event");
    const callOrder1 = fn1.mock.invocationCallOrder[0];
    const callOrder2 = fn2.mock.invocationCallOrder[0];

    expect(callOrder1).toBeGreaterThan(callOrder2);

    fn1.mockReset();
    fn2.mockReset();
    emitter.emit("event");

    expect(fn1).not.toHaveBeenCalled();
    expect(fn2).not.toHaveBeenCalled();
  });

  it("should add and remove a listener to the beginning of the list (prependListener)", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.prependListener("event", fn1);
    emitter.prependOnceListener("event", fn2);

    emitter.emit("event");
    const callOrder1 = fn1.mock.invocationCallOrder[0];
    const callOrder2 = fn2.mock.invocationCallOrder[0];

    expect(callOrder1).toBeGreaterThan(callOrder2);

    fn1.mockReset();
    fn2.mockReset();
    emitter.emit("event");

    expect(fn1).toHaveBeenCalled();
    expect(fn2).not.toHaveBeenCalled();
  });

  it("should add and remove a listener to the beginning of the list (prependOnceListener)", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.prependOnceListener("event", fn1);
    emitter.prependOnceListener("event", fn2);

    emitter.emit("event");
    const callOrder1 = fn1.mock.invocationCallOrder[0];
    const callOrder2 = fn2.mock.invocationCallOrder[0];

    expect(callOrder1).toBeGreaterThan(callOrder2);

    fn1.mockReset();
    fn2.mockReset();
    emitter.emit("event");

    expect(fn1).not.toHaveBeenCalled();
    expect(fn2).not.toHaveBeenCalled();
  });

  it("should add and remove a listener to the emply list", () => {
    const fn = jest.fn();
    emitter.prependOnceListener("event", fn);

    emitter.emit("event");
    expect(fn).toHaveBeenCalled();

    fn.mockReset();
    emitter.emit("event");

    expect(fn).not.toHaveBeenCalled();
  });

  it("should throw if listener is not a function", () => {
    expect(() => emitter.prependOnceListener("event", "string")).toThrow(
      "Listener should be a function, received string"
    );
  });
});

describe("listeners", () => {
  it("should return an empty array if no listeners are registered", () => {
    const result = emitter.listeners("event");

    expect(Array.isArray(result)).toBe(true);
    expect(result).toHaveLength(0);
  });

  it("should return a copy of the listener array", () => {
    const fn1 = jest.fn();
    const fn2 = jest.fn();

    emitter.prependListener("event", fn1);
    emitter.prependListener("event", fn2);

    const listeners = emitter.listeners("event");

    expect(listeners).toEqual([fn2, fn1]);
    // reference comparison
    expect(listeners).not.toBe(emitter.listeners("event"));
  });

  it("should return a copy of the listener array (modifying returned array)", () => {
    const listener = jest.fn();
    emitter.prependListener("event", listener);

    const copy = emitter.listeners("event");
    copy.push(jest.fn());

    expect(emitter.listeners("event")).toEqual([listener]);
  });
});
