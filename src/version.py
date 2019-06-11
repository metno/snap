#! /usr/bin/env python3
from subprocess import check_output, DEVNULL
import sys


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
    git_tags = check_output(["git", "tag", "--points-at"])
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
    output = check_output(["git", "ls-remote", "--tags"], stderr=DEVNULL)
    output = output.decode("utf-8").splitlines()

    for line in output:
        tagcommit, taghash = line.split()
        if taghash == "refs/tags/" + tag and tagcommit == commithash:
            return True
    return False


if __name__ == "__main__":
    if git_dirty():
        print("The current directory is (git) dirty", file=sys.stderr)
        print("git.DIRTY")
        exit(1)

    tags = git_tags()
    if len(tags) == 0:
        commithash = git_hash()
        print("git.hash.{}".format(commithash))
        exit(1)

    if len(tags) == 2:
        print("More than one tag detected, using tag {}".format(tags[0]),
              file=sys.stderr)

    commithash = git_hash(short_hash=False)
    remote_consistent = git_tag_remote_consistent(tags[0], commithash)
    if not remote_consistent:
        # Local tag not consistent with remote tag
        print("Tag was not found on the remote server matching the local hash",
              file=sys.stderr)

        print("git.BADTAG+git.hash.{}".format(git_hash()))
        exit(1)

    print(tags[0])
    exit(0)
