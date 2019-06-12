#! /usr/bin/env python3
import argparse
from subprocess import call, check_output, DEVNULL
import sys

TIMEOUT = 30  # [seconds]


def no_git_available():
    """
    Checks whether git is available
    """
    try:
        call(["git", "--version"], stdout=DEVNULL)
    except FileNotFoundError:
        return True
    return False


def git_dirty():
    """
    Checks whether we are on a dirty git directory, which may
    contain unwanted artifacts or uncommited items
    """
    git_status = check_output(["git", "status", "--porcelain"])
    dirty = git_status != b''
    return dirty


def git_tags():
    """
    Gets the latest git tag that points at current HEAD
    """
    git_tags = check_output(["git", "tag", "--points-at", "HEAD"])
    tags = git_tags.decode("utf-8").splitlines()
    return tags


def git_hash(short_hash=True):
    """
    Gets the latest git hash
    """
    command = ["git", "rev-parse", "--verify", "HEAD"]
    if short_hash:
        command.append("--short")
    output = check_output(command)

    return output.decode("utf-8").strip()


def git_tag_remote_consistent(tag, commithash):
    """
    Checks the remote for tags, to ensure the local tags have been
    pushed to remote.
    """
    # stderr contains the "From ..." line, which we ignore
    try:
        output = check_output(["git", "ls-remote", "--tags"],
                              stderr=DEVNULL,
                              timeout=TIMEOUT)
    except TimeoutExpired:
        print("Could not connect to remote in a suitable amount of time",
              file=sys.stderr)
        return None

    output = output.decode("utf-8").splitlines()

    for line in output:
        tagcommit, taghash = line.split()
        if taghash == "refs/tags/" + tag and tagcommit == commithash:
            return True
    return False


if __name__ == "__main__":
    argparser = argparse.ArgumentParser(
            description="Creates a VERSION string that can be embedded in a"
                        + " program. The preffered VERSION consists of a"
                        + " git tag, but in untagged releases/builds, VERSION"
                        + " will instead consist of the commit hash. VERSION"
                        + " is returned on STDOUT, errors will return a"
                        + " non-zero return code.")
    argparser.add_argument("--skip-remote-check",
                           action="store_true",
                           help="Do not check whether remote has the tag, or"
                                + " that the tag matches the same commit as"
                                + "the local repo")

    args = argparser.parse_args()

    if no_git_available():
        print("git is not available", file=sys.stderr)
        print("git.unavailable")
        exit(1)

    if git_dirty():
        print("The current directory is (git) dirty", file=sys.stderr)
        print("git.DIRTY")
        exit(1)

    tags = git_tags()
    if len(tags) == 0:
        commithash = git_hash()
        print("git.hash.{}".format(commithash))
        exit(0)

    if len(tags) == 2:
        print("More than one tag detected, using tag {}".format(tags[0]),
              file=sys.stderr)

    commithash = git_hash(short_hash=False)
    if not args.skip_remote_check:
        remote_consistent = git_tag_remote_consistent(tags[0], commithash)
    else:
        remote_consistent = None
    if remote_consistent is None:
        print("git.tag.UNKNOWN_CONSISTENT+git.hash.{}".format(git_hash()))
        exit(1)
    if not remote_consistent:
        # Local tag not consistent with remote tag
        print("Tag was not found on the remote server matching the local hash",
              file=sys.stderr)

        print("git.tag.BAD+git.hash.{}".format(git_hash()))
        exit(1)

    print(tags[0])
    exit(0)
