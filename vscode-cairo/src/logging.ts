import { Event, LogLevel, LogOutputChannel, ViewColumn } from "vscode";

/**
 * An adapter for {@link LogOutputChannel} that allows processing messages
 * before they are sent to the proper channel.
 */
abstract class LogOutputChannelAdapter implements LogOutputChannel {
  public constructor(private readonly parent: LogOutputChannel) {}

  protected processMessage(message: string): string {
    return message;
  }

  get logLevel(): LogLevel {
    return this.parent.logLevel;
  }

  get onDidChangeLogLevel(): Event<LogLevel> {
    return this.parent.onDidChangeLogLevel;
  }

  trace(message: string, ...args: unknown[]): void {
    this.parent.trace(this.processMessage(message), ...args);
  }

  debug(message: string, ...args: unknown[]): void {
    this.parent.debug(this.processMessage(message), ...args);
  }

  info(message: string, ...args: unknown[]): void {
    this.parent.info(this.processMessage(message), ...args);
  }

  warn(message: string, ...args: unknown[]): void {
    this.parent.warn(this.processMessage(message), ...args);
  }

  error(error: string | Error, ...args: unknown[]): void {
    if (error instanceof Error) {
      error.message = this.processMessage(error.message);
      return this.parent.error(error, ...args);
    }

    return this.parent.error(this.processMessage(error), ...args);
  }

  get name(): string {
    return this.parent.name;
  }

  append(value: string): void {
    this.parent.append(this.processMessage(value));
  }

  appendLine(value: string): void {
    this.parent.appendLine(this.processMessage(value));
  }

  replace(value: string): void {
    this.parent.replace(value);
  }

  clear(): void {
    this.parent.clear();
  }

  show(column?: unknown, preserveFocus?: unknown): void {
    this.parent.show(column as ViewColumn, preserveFocus as boolean);
  }

  hide(): void {
    this.parent.hide();
  }

  dispose(): void {
    this.parent.dispose();
  }
}

/**
 * A {@link LogOutputChannel} that allows creating child spans.
 */
export class RootLogOutputChannel extends LogOutputChannelAdapter {
  public span(spanName: string): SpanLogOutputChannel {
    return new SpanLogOutputChannel(this, spanName);
  }
}

/**
 * A {@link LogOutputChannel} that prepends the span name to all messages.
 */
export class SpanLogOutputChannel extends LogOutputChannelAdapter {
  public constructor(
    parent: LogOutputChannel,
    public readonly spanName: string,
  ) {
    super(parent);
  }

  protected override processMessage(message: string): string {
    return `${this.spanName}: ${message}`;
  }
}
